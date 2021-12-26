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

(define empty-registers (make-registers 0 0 0 0));)
(define (print-registers registers)
  (format #t "w=~a,x=~a,y=~a,z=~a\n" (reg-w registers) (reg-x registers) (reg-y registers) (reg-z registers)))

(define-immutable-record-type <state>
   (make-state-with-molecules regs input molecules)
   state?
   (regs state-regs set-state-regs)
   (input state-input set-state-input)
   (molecules state-molecules! set-state-molecules!))

(define (make-state regs input)
     (make-state-with-molecules regs input (make-hash-table)))

(define (get-reg-or-immediate registers val)
  (cond
      ((string=? "w" val) (reg-w registers))
      ((string=? "x" val) (reg-x registers))
      ((string=? "y" val) (reg-y registers))
      ((string=? "z" val) (reg-z registers))
      (else (string->number val))))

(define (set-reg registers name val)
  (cond
      ((string=? "w" name) (set-reg-w registers val))
      ((string=? "x" name) (set-reg-x registers val))
      ((string=? "y" name) (set-reg-y registers val))
      ((string=? "z" name) (set-reg-z registers val))
      (else (error (format #f "unknown register '~a'" name)))))

(define (read-input input)
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
        (make-state
         (set-reg regs (car args) val)
         input))))

(define (bin-op op args)
   (lambda (regs input)
     (let* ((a (first args))
            (b (second args))
            (aval (get-reg-or-immediate regs a))
            (bval (get-reg-or-immediate regs b))
            (result (op aval bval)))
      (make-state
       (set-reg regs a result)
       input))))

(define (run program input)
  (fold
    (lambda (op state)
      ;(print-registers (state-regs state))
      (op (state-regs state) (state-input state)))
    (make-state empty-registers input)
    program))

(define (op-inp args)
   (lambda (regs input)
     (let*-values (((val input) (read-input input)))
        (make-state
         (set-reg regs (car args) val)
         input))))

(define (op-mul a b)
      (cond
       ((and (number? a) (= 0 a))     0)
       ((and (number? b) (= 0 b))     0)
       ((and (number? a) (= 1 a))     b)
       ((and (number? b) (= 1 b))     a)
       ((and (number? b) (number? a)) (* a b))
       ((and (list? a) (symbol? (car a)) (eq? a 'MUL)
             (list? b) (symbol? (car b)) (eq? b 'MUL))
        (append '(MUL) (cdr a) (cdr b)))
       ((and (list? a) (symbol? (car a)) (eq? a 'MUL))
        (append '(MUL) (cdr a) b))
       ((and (list? b) (symbol? (car b)) (eq? b 'MUL))
        (append '(MUL) a (cdr b)))
       (else                          (list 'MUL a b))))

(define (op-div a b)
      (cond
       ((and (number? a) (= 0 a))     0)
       ((and (number? b) (= 1 b))     a)
       ((and (number? a) (number? b)) (quotient a b))
       (else                          (list 'DIV a b))))

(define (op-mod a b)
      (cond
       ((and (number? a) (= 0 a))     0)
       ((and (number? b) (= 1 b))     a)
       ((and (number? a) (number? b)) (remainder a b))
       (else                          (list 'MOD a b))))

(define (op-add a b)
      (cond
       ((and (number? a) (= 0 a))     b)
       ((and (number? b) (= 0 b))     a)
       ((and (number? a) (number? b)) (+ a b))
       ((and (list? a) (symbol? (car a)) (eq? a 'ADD)
             (list? b) (symbol? (car b)) (eq? b 'ADD))
        (append '(ADD) (cdr a) (cdr b)))
       ((and (list? a) (symbol? (car a)) (eq? a 'ADD))
        (append '(ADD) (cdr a) b))
       ((and (list? b) (symbol? (car b)) (eq? b 'ADD))
        (append '(ADD) a (cdr b)))
       (else                          (list 'ADD a b))))

(define (op-eql a b)
     (cond
      ((and (number? a) (number? b)) (if (= a b) 1 0))
      ((and (number? a) (symbol? b)) (if (or (< a 1) (< 9 a)) 0
                                         (list 'EQL a b)))
      ((and (number? b) (symbol? a)) (if (or (< b 1) (< 9 b)) 0
                                         (list 'EQL a b)))
      (else                          (list 'EQL a b))))

(define (line->op line)
  (let* ((full-op (string-split line #\space))
         (op-name (car full-op))
         (op-args (cdr full-op))
         (_ (dbg "ops=" (list op-name op-args)))
         (op (cond
              ((string= op-name "inp") (op-inp op-args))
              ((string= op-name "add") (bin-op op-add op-args))
              ((string= op-name "mul") (bin-op op-mul op-args))
              ((string= op-name "mod") (bin-op op-mod op-args))
              ((string= op-name "div") (bin-op op-div op-args))
              ((string= op-name "eql") (bin-op op-eql op-args)))))
    op))

(define (parse-program port)
  (let parse-program-rec ((line (read-line port)) (result '()))
    (cond
     ((eof-object? line) (reverse result))
     (else
      (parse-program-rec (read-line port)
                         (cons (line->op line) result))))))

(define (symbolic-analysis program)
  (let ((end-state  (run program '( a b c d e f g h i j k l m n))))
    (reg-z (state-regs end-state))))

(define (num->monad-input num)
  (let num->list-rec ((num num) (result '()))
     (cond
      ((= 0 num) result)
      (else
        (let* ((last-digit (remainder num 10))
               (other-digits (quotient num 10)))
         (cond
          ((= 0 last-digit) #f) ;; if it contains a digit 0 it is not a MONAD number
          (else (num->list-rec other-digits (cons last-digit result)))))))))

(define (valid-end-state? state)
  (= 0 (reg-z (state-regs state))))

(define (highest-fourtenn-digit-validated program from)
   (let highest-rec ((num from))
     (let* ((num-as-list (num->monad-input num))
            (final-state (if num-as-list (run program num-as-list) #f))
            (valid? (if final-state (valid-end-state? final-state) #f)))
        (if (= 0 (remainder num 10000)) (dbg "h:" (list num valid?)))
        (cond
         (valid? num)
         (else (highest-rec (1- num)))))))


(use-modules (statprof))
(define-public (main args)
   (let*-values (
                 ((program) (parse-program (current-input-port)))
                 ;(_ (statprof (lambda () (highest-fourtenn-digit-validated program (+ 5000000 (expt 10 13))))))
                 ((result1) (symbolic-analysis program))
                 ((result2) "UNIMP"))
      (format #t "result1: ~a\n" result1)
      (format #t "result2: ~a\n" result2)))


;;(define program (parse-program (open-input-file "inputs/input")))
;;(define res-sym (symbolic-analysis program))
;;
;;(define (depth x)
;;  (cond
;;    ((not (list? x)) 1)
;;    ((null? x) 0)
;;    (else
;;      (1+ (max (depth (car x))
;;              (depth (cdr x)))))))
;;
;;(define (op-depth x)
;;  (cond
;;    ((not (list? x)) 1) ; ATOM
;;    ((null? x) 0) ; should never happen
;;    (else
;;     (let* ((op  (car x))
;;            (args (cdr x))
;;            (args-depth (apply max (map depth args))))
;;      (1+ args-depth)))))
;;
;;(depth res-sym)
;;(op-depth res-sym)
;;(print-tree res-sym 0)
;;(define (print-indent n)
;;  (for-each
;;   (lambda (_) (display " "))
;;   (iota n)))
;;
;;(define (print-tree obj indent)
;;  (cond
;;   ((>= indent 20) (print-indent indent) (display "...\n"))
;;   ((null? obj))
;;   ((list? obj) (map (cut print-tree <> (1+ indent)) obj))
;;   (else (print-indent indent) (display obj) (display "\n")))
;;  #t)
;;
