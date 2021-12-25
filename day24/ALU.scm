(define-module (adventofcode2021 day24 ALU)
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

(define (dbg t v) (format #t "~a~A\n" t v) (force-output))
(define (dbgn t v) (format #t "~a~a  #  " t v) (force-output))

(define-immutable-record-type <registers>
   (make-registers w x y z)
   registers?
   (w reg-w set-reg-w)
   (x reg-x set-reg-x)
   (y reg-y set-reg-y)
   (z reg-z set-reg-z))

(define empty-registers (make-registers 0 0 0 0))
(define (print-registers registers)
  (format #t "w=~a,x=~a,y=~a,z=~a\n" (reg-w registers) (reg-x registers) (reg-y registers) (reg-z registers)))

(define-immutable-record-type <state>
   (make-state regs input)
   state?
   (regs state-regs set-state-regs)
   (input state-input set-state-input))

(define (get-reg registers name)
  (case name
      ((#\w) (reg-w registers))
      ((#\x) (reg-x registers))
      ((#\y) (reg-y registers))
      ((#\z) (reg-z registers))))

(define (set-reg registers name val)
  (case name
      ((#\w) (set-reg-w registers val))
      ((#\x) (set-reg-x registers val))
      ((#\y) (set-reg-y registers val))
      ((#\z) (set-reg-z registers val))))

(define (read-input input)
  (dbg "input=" input)
  (cond
   ((null? input)
    (values #f
            '()))
   (else
    (values (car input)
            (cdr input)))))

(define (inp args)
   (lambda (regs input)
     (let*-values (((val input) (read-input input)))
        (dbg "val=" val)
        (make-state
         (set-reg regs (car args) val)
         input))))

(define (bin-op op args)
   (lambda (regs input)
     (let* ((a (first args))
            (b (second args))
            (_ (dbg "a,b=" (list a b)))
            (aval (get-reg regs a))
            (bval (get-reg regs b))
            (_ (dbg "aval,bval=" (list aval bval)))
            (result (op aval bval)))
      (make-state
       (set-reg regs a result)
       input))))

(define (run program input)
  (fold
    (lambda (op state)
      (print-registers (state-regs state))
      (op (state-regs state) (state-input state)))
    (make-state empty-registers input)
    program))

(define (line->op line)
  (let* ((full-op (string-split line #\space))
         (op-name (car full-op))
         (op-args (cdr full-op))
         (_ (dbg "ops=" (list op-name op-args)))
         (op-args (map (cut string-ref <> 0)
                       op-args))
         (op (cond
              ((string= op-name "inp") (inp op-args))
              ((string= op-name "add") (bin-op + op-args))
              ((string= op-name "mul") (bin-op * op-args))
              ((string= op-name "mod") (bin-op remainder op-args))
              ((string= op-name "div") (bin-op quotient op-args))
              ((string= op-name "eql") (bin-op (lambda (a b)
                                                 (if (= a b) 1 0))
                                               op-args)))))
    op))

(define (parse-program port)
  (let parse-program-rec ((line (read-line port)) (result '()))
    (cond
     ((eof-object? line) (reverse result))
     (else
      (parse-program-rec (read-line port)
                         (cons (line->op line) result))))))

(define (valid-model-input? input)
    (not (any (cut char=? #\0 <>) input)))

(define (next-model-number-candidate num)
  (let next-rec ((num num))
    (let ((num-as-list (string->list (number->string num))))
      (cond
       ((< num (expt 10 13))
        (error (format #f "nombre ~a avec moins de 14 chiffres" num)))
       ((valid-model-input? num-as-list)
        (values num num-as-list))
       (else (next-rec (1- num)))))))

(define (valid-end-state? state)
  (= 0 reg-z (state-regs state)))

(define (highest-fourtenn-digit-validated program)
   (let highest-rec ((num (expt 10 14)))
     (let*-values (((num num-as-list) (next-model-number-candidate (1- num)))
                   (_ (dbg "E" (list num num-as-list)))
                   (_ (dbg "f" num-as-list))
                   ((num-as-list) (map digit->number num-as-list))
                   (_ (dbg "F" num-as-list))
                   ((final-state) (run program num-as-list)))
             (cond
              ((valid-end-state? final-state) num)
              (else (highest-rec num))))))

(define-public (main args)
   (let*-values (
                 ((program) (parse-program (current-input-port)))
                 ((result1) (highest-fourtenn-digit-validated program))
                 ((result2) "UNIMP"))
      (format #t "result1: ~a\n" result1)
      (format #t "result2: ~a\n" result2)))


