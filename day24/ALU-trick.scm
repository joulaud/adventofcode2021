(define-module (adventofcode2021 day24 ALU-trick)
    #:use-module (adventofcode2021 day24 utils)
    #:use-module (ice-9 rdelim)
    #:use-module (ice-9 match)
    #:use-module (ice-9 format)
    #:use-module (srfi srfi-1) ; lists library
    #:use-module (srfi srfi-2) ; and-let*
    #:use-module (srfi srfi-11) ; let*-values
    #:use-module (srfi srfi-9)
    #:use-module (srfi srfi-9 gnu) ; immutable records
    #:use-module (srfi srfi-41) ; Streams
    #:use-module (srfi srfi-43) ; Vectors iterators
    #:use-module (srfi srfi-26) ; cut (specializing parameters, currying alternative)
    #:use-module (srfi srfi-69) ; hash tables
    #:use-module (srfi srfi-171)) ; transducers

(use-modules (ice-9 rdelim))
(use-modules (ice-9 match))
(use-modules (ice-9 format))
(use-modules (ice-9 vlist))
(use-modules (srfi srfi-2)) ; and-let*
(use-modules (srfi srfi-1)) ; lists library
(use-modules (srfi srfi-11)) ; let*-values
(use-modules (srfi srfi-9))
(use-modules (srfi srfi-9 gnu)) ; immutable records
(use-modules (srfi srfi-41))
(use-modules (srfi srfi-26)) ; cut (specializing parameters, currying alternative)
(use-modules (srfi srfi-69)) ; hash tables
(use-modules (system base compile))

(define (dbg t v) (format #t "~a~A\n" t v) (force-output))
(define (dbgn t v) (format #t "~a~a  #  " t v) (force-output))

;; Careful analysis made by others than me on the Internet shows that
;; the provided input for this puzzle is composed of 14 similar parts,
;; one for each input digit.
;;
;; Each part have exactly the same form (see fourth-analysis.txt file
;; for the example) and can be seen as a single megainstruction with
;; 5 parameters :
;; - z register from the previous state
;; - current input digit
;; - 3 hardcoded values we will call addx divz and addy because that is
;;   where they appear.
;;
;; The meaning of this megainstruction is :
;; if input != z mod 26 + @x      then 26*(z div @z) + @y  else (z div @z) end
(define (megainstruction-exec add-x div-z add-y input zprev)
     (let* ((zintermediate (quotient zprev div-z)))
       (cond
        ((= input (+ (remainder zprev 26) add-x))
         zintermediate)
        (else
         (+ (* 26 zintermediate) div-z)))))

(define-immutable-record-type <megainstruction>
  (make-inst addx divz addy)
  inst?
  (addx inst-addx set-inst-addx)
  (divz inst-divz set-inst-divz)
  (addy inst-addy set-inst-addy))

(define (inst-exec inst input prev)
  (megainstruction-exec (inst-addx inst) (inst-divz inst) (inst-addy inst) input prev))

(define (inst-func inst)
 (lambda (input prev)
  (megainstruction-exec (inst-addx inst) (inst-divz inst) (inst-addy inst) input prev)))

(define (parse-line line inst)
  (let* ((operand (substring line 6))
         (operand (string->number operand)))
    (cond
     (operand))
    (cond
     ((and operand (string-prefix? "add x " line))
      (set-inst-addx inst operand))
     ((and operand (string-prefix? "add y " line))
      (set-inst-addy inst operand))
     ((and operand (string-prefix? "div z " line))
      (set-inst-divz inst operand))
     (else inst))))

(define (parse-program port)
  (let parse-program-rec ((line (read-line port)) (inst (make-inst #f #f #f))
                                                  (program '()))
    (cond
     ((eof-object? line) (cdr (reverse (cons inst program))))
     ((string-prefix? "inp " line)
      (dbg "inst=" inst)
      (parse-program-rec (read-line port)
                         (make-inst #f #f #f)
                         (cons inst program)))
     (else
        (parse-program-rec (read-line port)
                           (parse-line line inst)
                           program)))))

(define (program->funcs program)
   (map inst-func program))

(define (highest-digits-rec program z depth digit-list)
  (cond
   ((null? program)
    (if (= 0 z) 1 #f))
   (else
     (let loop ((digits digit-list))
       (if (< depth 4)
           (dbg "depth,digits=" (list depth digits)))
       (cond
        ((null? digits) #f)
        (else (let* ((inst (car program))
                     (digit (car digits))
                     (newz (inst digit z))
                     (subresult (highest-digits-rec (cdr program) newz (1+ depth) digit-list))
                     (result (if subresult (+ digit (* 10 subresult))
                                           (loop (cdr digits)))))
                 result)))))))

(define (highest-digits program)
    (highest-digits-rec program 0 0 (iota 9 9 -1)))

(use-modules (statprof))
(define-public (main args)
   (let*-values (
                 ((program) (program->funcs (parse-program (current-input-port))))
                 ((result1) (highest-digits program))
                 ((result2) "UNIMP")) ;(run program '(1 2 3 4 5 6 7 8 9 9 1 2 3 4 5))))
      (format #t "result1: ~a\n" result1)
      (format #t "result2: ~a\n" result2)))
