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
(use-modules (system base compile))

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
   ;; WARNING: putting mutable hashes in immutable-record is non-sense
   ;; but I will not rewrite this so cross your fingers.
   (make-state-with-exp-hash regs input exp2sym sym2exp counter)
   state?
   (regs state-regs set-state-regs)
   (input state-input set-state-input)
   (exp2sym state-exp2sym! set-state-exp2sym!)
   (sym2exp state-sym2exp! set-state-sym2exp!)
   (counter state-counter set-state-counter))

(define (make-state regs input counter)
     (make-state-with-exp-hash regs input (make-hash-table) (make-hash-table) counter))

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
   (lambda (state)
       (let*-values (((val input) (read-input (state-input state))))
         (let* ((state (set-state-regs state (set-reg (state-regs state) (car args) val)))
                (state (set-state-input state input))
                (state (set-state-counter state (1+ (state-counter state)))))
           state))))

(define (bin-op op args)
   (lambda (state)
     (let* (
            (regs (state-regs state))
            (counter (state-counter state))
            (a (first args))
            (b (second args))
            (aval (get-reg-or-immediate regs a))
            (bval (get-reg-or-immediate regs b))
            (result (op aval bval))
            (state (set-state-regs state (set-reg regs a result)))
            (state (set-state-counter state (1+ counter))))
       state)))

(define (rewrite-ssa-style-reg state reg)
   (let* ((regs    (state-regs state))
          (counter (state-counter state))
          (exp2sym (state-exp2sym! state))
          (sym2exp (state-sym2exp! state))
          (val (get-reg-or-immediate regs reg)))
    (cond
     ((symbol? val)
      state)
     ((number? val)
      state)
     ((list? val)
      (let* ((existing-sym (hash-table-ref/default exp2sym val #f))
             (new-sym
               (if existing-sym existing-sym
                   (let ((new-symbol (string->symbol (string-append
                                                      reg
                                                      "-"
                                                      (number->string counter)))))
                      (hash-table-set! exp2sym val new-symbol)
                      (hash-table-set! sym2exp new-symbol val)
                      new-symbol))))
         (set-state-regs state (set-reg regs reg new-sym))))
     (else (error (format #f "'~a' is not a valid register content" val))))))

(define (rewrite-ssa-style state)
  (let* ((state (rewrite-ssa-style-reg state "w"))
         (state (rewrite-ssa-style-reg state "x"))
         (state (rewrite-ssa-style-reg state "y"))
         (state (rewrite-ssa-style-reg state "z")))
    state))

(define (run program input)
  (fold
    (lambda (op state)
      ;(print-registers (state-regs state))
      (op state))
    (make-state empty-registers input 0)
    program))

(define (run-from-registers program input registers)
  (fold
    (lambda (op state)
      ;(print-registers (state-regs state))
      (op state))
    (make-state registers input 0)
    program))

(define (run-ssa program input)
  (fold
    (lambda (op state)
      ;(print-registers (state-regs state))
      (rewrite-ssa-style (op state)))
    (make-state empty-registers input 0)
    program))

(define op-inp inp)

(define (op-mul a b)
      (cond
       ((and (number? a) (= 0 a))     0)
       ((and (number? b) (= 0 b))     0)
       ((and (number? a) (= 1 a))     b)
       ((and (number? b) (= 1 b))     a)
       ((and (number? b) (number? a)) (* a b))
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
       ((and (number? b) (= 1 b))     0)
       ((and (number? a) (number? b)) (remainder a b))
       (else                          (list 'MOD a b))))

(define (op-add a b)
      (cond
       ((and (number? a) (= 0 a))     b)
       ((and (number? b) (= 0 b))     a)
       ((and (number? a) (number? b)) (+ a b))
       (else                          (list 'ADD a b))))

(define (op-eql a b)
     (cond
      ((and (number? a) (number? b)) (if (= a b) 1 0))
      ;((and (number? a) (symbol? b)) (if (or (< a 1) (< 9 a)) 0
      ;                                   (list 'EQL a b)))
      ;((and (number? b) (symbol? a)) (if (or (< b 1) (< 9 b)) 0
      ;                                   (list 'EQL a b)))
      (else                          (list 'EQL a b))))

(define (line->op line)
  (let* ((full-op (string-split line #\space))
         (op-name (car full-op))
         (op-args (cdr full-op))
         (op (cond
              ((string= op-name "inp") (op-inp op-args))
              ((string= op-name "add") (bin-op op-add op-args))
              ((string= op-name "mul") (bin-op op-mul op-args))
              ((string= op-name "mod") (bin-op op-mod op-args))
              ((string= op-name "div") (bin-op op-div op-args))
              ((string= op-name "eql") (bin-op op-eql op-args)))))
    op))

(define (parse-programs port)
  (let parse-program-rec ((line (read-line port)) (program '()) (programs '()))
    (cond
     ((eof-object? line) (reverse (cons (reverse program) programs)))
     (else
       (let* ((inp? (string-prefix? "inp " line))
              (programs (if inp? (cons (reverse program) programs) programs))
              (program  (if inp? '() program)))
        (parse-program-rec (read-line port)
                           (cons (line->op line) program)
                           programs))))))

(define (symbolic-analysis program)
  (run-ssa program '( a b c d e f g h i j k l m n)))

(define (state->z-expression state)
   (let* ((z-content (reg-z (state-regs state)))
          (sym2exp  (state-sym2exp! state))
          (z-exp (hash-table-ref sym2exp z-content)))
       z-exp))

(define (resolve-state-symbol state symbol)
  (let ((sym2exp (state-sym2exp! state)))
    (let resolve-state-symbol-rec
         ((syms (list symbol))
          (result '()))
      (cond
       ((null? syms) result)
       (else (let* ((cur (car syms))
                    (result (cons cur result))
                    (expr (hash-table-ref sym2exp cur))
                    (rest (cdr syms))
                    (syms (push-syms-from-expr rest expr)))
                (resolve-state-symbol-rec syms result)))))))

(define (push-syms-from-expr syms expr)
  (let* ((op (car expr))
         (rest (cdr expr))
         (rest (filter
                (lambda (cur) (and (symbol? cur) (string-index (symbol->string cur) #\-)))
                rest)))
     (append rest syms)))

(define (program->ordered-bindings program)
   (let* ((end-state (symbolic-analysis program))
          ;(w         (reg-w (state-regs end-state)))
          ;(x         (reg-x (state-regs end-state)))
          ;(y         (reg-y (state-regs end-state)))
          (z         (reg-z (state-regs end-state)))
          (ordered-symbols (resolve-state-symbol end-state z))
          (ordered-exprs
                     (map
                       (cute hash-table-ref (state-sym2exp! end-state) <>)
                       ordered-symbols))
          (ordered-bindings (delete-duplicates (zip ordered-symbols ordered-exprs))))
      (values ordered-bindings z)))

(define-immutable-record-type <range>
  (make-range expression borne-inf borne-sup)
  range?
  (expression range-expr set-range-expr)
  (borne-inf range-min set-range-min)
  (borne-sup range-max set-range-max))

(define (range-analysis ordered-bindings)
  ;; ORDERED-BINDINGS are in single assignement form
  ;; all expr have only previous symbols or immediate values as arguments
  (define (expr-sym? sym)
      (string-index (symbol->string sym) #\-))
  (define (get-range-for-val sym2ranges val)
    (cond
     ((number? val) (make-range val val val))
     ((and (symbol? val) (expr-sym? val))
      (hash-table-ref sym2ranges val))
     ((symbol? val)
      ;; bare symbols (from 'a to 'n) represent non-zero digits
      (make-range val 1 9))
     (else (error "'~a' is not a correct value to be in SSA expr"))))
  (define (rg-mul r1 r2)
     (let* ((r1-expr (range-expr r1)) (r1-min (range-min r1)) (r1-max (range-max r1))
            (r2-expr (range-expr r2)) (r2-min (range-min r2)) (r2-max (range-max r2)))
       (cond
        ;; multiplication par 0, range-collapse
        ((or (= 0 r1-min r1-max)
             (= 0 r2-min r2-max))
         (make-range 0 0 0))
        ;; multiplications par 1, cas facile
        ((= 1 r1-min r1-max)
         r2)
        ((= 1 r2-min r2-max)
         r1)
        ;; seulement des nombres positifs, ça reste précis
        ((and (>= r1-min 0) (>= r2-min 0))
         (make-range
           #f ;(list 'MUL r1-expr r2-expr)
           (* r1-min r2-min)
           (* r1-max r2-max)))
        ;; aussi des nombres négatifs, je fais une surestimation grossière
        ((or (< r1-min 0) (< r2-min 0))
         (let ((absmax (* (max (abs r1-min) (abs r1-max))
                          (max (abs r2-min) (abs r2-max)))))
           (make-range
             #f ;(list 'MUL r1-expr r2-expr)
             (- absmax)
             absmax))))))
  (define (rg-add r1 r2)
     (let* ((r1-expr (range-expr r1)) (r1-min (range-min r1)) (r1-max (range-max r1))
            (r2-expr (range-expr r2)) (r2-min (range-min r2)) (r2-max (range-max r2)))
       (make-range
         #f ;(list 'ADD r1-expr r2-expr)
         (+ r1-min r2-min)
         (+ r1-max r2-max))))
  (define (rg-eql r1 r2)
     (let* ((r1-expr (range-expr r1)) (r1-min (range-min r1)) (r1-max (range-max r1))
            (r2-expr (range-expr r2)) (r2-min (range-min r2)) (r2-max (range-max r2)))
       (cond
        ;; cas disjoint, doit toujours être 0
        ((or (< r1-max r2-min) (< r2-max r1-min))
         (dbg "disjoints: " (list r1 r2))
         (make-range 0 0 0))
        ;; cas égaux, doit toujours être 1
        ((and (= r1-min r2-min) (= r1-max r2-max))
         (dbg "égaux:     " (list r1 r2))
         (make-range 1 1 1))
        ;; cas indéfinis, soit 0 soit 1
        (else
         (dbg "indéfini:  " (list r1 r2))
         (make-range
          #f ;(list 'EQL r1-expr r2-expr)
          0 1)))))
  (define (rg-div r1 r2)
     (let* ((r1-expr (range-expr r1)) (r1-min (range-min r1)) (r1-max (range-max r1))
            (r2-expr (range-expr r2)) (r2-min (range-min r2)) (r2-max (range-max r2)))
       (cond
        ;; division par 1, on ne change rien
        ((and (number? r2-expr) (= 1 r2-expr))
         r1)
        ;; seulement des nombres positifs, ça reste précis
        ((and (>= r1-min 0) (> r2-min 0))
         (make-range
          #f ;(list 'DIV r1-expr r2-expr)
          (quotient r1-min r2-max)
          (quotient r1-max r2-min)))
        ;; je n'ai pas réfléchi aux nombres négatifs, j'espère que le cas ne se produira pas
        ;; Je considèere qu'au pire la valeur absolue d'une division
        ;; est toujours inférieure à celle d'une multiplication
        (else
         (let ((absmax (* (max (abs r1-min) (abs r1-max))
                          (max (abs r2-min) (abs r2-max)))))
            (make-range
             #f ;(list 'DIV r1-expr r2-expr)
             (- absmax)
             absmax))))))
  (define (rg-mod r1 r2)
     (let* ((r1-expr (range-expr r1)) (r1-min (range-min r1)) (r1-max (range-max r1))
            (r2-expr (range-expr r2)) (r2-min (range-min r2)) (r2-max (range-max r2)))
       (cond
        ;; division par 1, le reste est toujours 0
        ((and (number? r2-expr) (= 1 r2-expr))
         (make-range 0 0 0))
        ;; r1 est dans l'intervalle, le modulo ne change rien
        ((< r1-max r2-min)
         r1)
        (else
         (make-range
          #f ;(list 'MOD r1-expr r2-expr)
          0 (1- r2-max))))))
  (let ((sym2ranges (make-hash-table)))
    (let range-analysis-rec ((lst ordered-bindings)
                             (result '()))
         (cond
          ((null? lst) result)
          (else
           (let* ((cur (car lst))
                  (sym (car cur))
                  (expr (cadr cur))
                  (op (first expr))
                  (r1 (get-range-for-val sym2ranges (second expr)))
                  (r2 (get-range-for-val sym2ranges (third expr)))
                  (res (cond
                        ((eq? op 'ADD) (rg-add r1 r2))
                        ((eq? op 'MUL) (rg-mul r1 r2))
                        ((eq? op 'MOD) (rg-mod r1 r2))
                        ((eq? op 'DIV) (rg-div r1 r2))
                        ((eq? op 'EQL) (rg-eql r1 r2))))
                  (res (if (or (number? (range-expr res))
                               (symbol? (range-expr res)))
                           (set-range-expr res (list sym (range-expr res)))
                           (set-range-expr res (list sym expr))))
                  (_ (hash-table-set! sym2ranges sym res)))
             (range-analysis-rec (cdr lst) (cons (range-expr res) result))))))))

(define (prune-bindings-reverse bnds initial-sym)
 (let ((track (make-hash-table)))
   (hash-table-set! track initial-sym #t)
   (let prune-rec ((bnds bnds) (result '()))
     (cond
      ((null? bnds) result)
      (else
       (let* ((bnd (car bnds))
              (sym (car bnd)))
        (cond
         ((hash-table-ref/default track sym #f)
          (let* ((expr (cadr bnd))
                 (_ (dbg "expr=" expr))
                 (symref1 (if (list? expr) (second expr) #f))
                 (symref2 (if (list? expr) (third expr)  #f)))
             (if (symbol? symref1) (hash-table-set! track symref1 #t))
             (if (symbol? symref2) (hash-table-set! track symref2 #t))
             (prune-rec (cdr bnds) (cons bnd result))))
         (else
          (dbg "drop sym=" sym)
          (prune-rec (cdr bnds) result)))))))))


(define (program->scheme-proc program)
  (let* ((ordered-bindings '((x-7 (EQL 10 a)) (x-8 (EQL x-7 0)) (y-16 (ADD a 2)) (y-17 (MUL y-16 x-8)) (x-22 (MOD y-17 26)) (x-24 (ADD x-22 10)) (x-25 (EQL x-24 b)) (x-26 (EQL x-25 0)) (y-34 (ADD b 4)) (y-35 (MUL y-34 x-26)) (y-29 (MUL 25 x-26)) (y-30 (ADD y-29 1)) (z-31 (MUL y-17 y-30)) (z-36 (ADD z-31 y-35)) (x-40 (MOD z-36 26)) (x-42 (ADD x-40 14)) (x-43 (EQL x-42 c)) (x-44 (EQL x-43 0)) (y-52 (ADD c 8)) (y-53 (MUL y-52 x-44)) (y-47 (MUL 25 x-44)) (y-48 (ADD y-47 1)) (z-49 (MUL z-36 y-48)) (z-54 (ADD z-49 y-53)) (x-58 (MOD z-54 26)) (x-60 (ADD x-58 11)) (x-61 (EQL x-60 d)) (x-62 (EQL x-61 0)) (y-70 (ADD d 7)) (y-71 (MUL y-70 x-62)) (y-65 (MUL 25 x-62)) (y-66 (ADD y-65 1)) (z-67 (MUL z-54 y-66)) (z-72 (ADD z-67 y-71)) (x-76 (MOD z-72 26)) (x-78 (ADD x-76 14)) (x-79 (EQL x-78 e)) (x-80 (EQL x-79 0)) (y-88 (ADD e 12)) (y-89 (MUL y-88 x-80)) (y-83 (MUL 25 x-80)) (y-84 (ADD y-83 1)) (z-85 (MUL z-72 y-84)) (z-90 (ADD z-85 y-89)) (x-94 (MOD z-90 26)) (x-96 (ADD x-94 -14)) (x-97 (EQL x-96 f)) (x-98 (EQL x-97 0)) (y-106 (ADD f 7)) (y-107 (MUL y-106 x-98)) (y-101 (MUL 25 x-98)) (y-102 (ADD y-101 1)) (z-95 (DIV z-90 26)) (z-103 (MUL z-95 y-102)) (z-108 (ADD z-103 y-107)) (x-112 (MOD z-108 26)) (x-115 (EQL x-112 g)) (x-116 (EQL x-115 0)) (y-124 (ADD g 10)) (y-125 (MUL y-124 x-116)) (y-119 (MUL 25 x-116)) (y-120 (ADD y-119 1)) (z-113 (DIV z-108 26)) (z-121 (MUL z-113 y-120)) (z-126 (ADD z-121 y-125)) (x-130 (MOD z-126 26)) (x-132 (ADD x-130 10)) (x-133 (EQL x-132 h)) (x-134 (EQL x-133 0)) (y-142 (ADD h 14)) (y-143 (MUL y-142 x-134)) (y-137 (MUL 25 x-134)) (y-138 (ADD y-137 1)) (z-139 (MUL z-126 y-138)) (z-144 (ADD z-139 y-143)) (x-148 (MOD z-144 26)) (x-150 (ADD x-148 -10)) (x-151 (EQL x-150 i)) (x-152 (EQL x-151 0)) (y-160 (ADD i 2)) (y-161 (MUL y-160 x-152)) (y-155 (MUL 25 x-152)) (y-156 (ADD y-155 1)) (z-149 (DIV z-144 26)) (z-157 (MUL z-149 y-156)) (z-162 (ADD z-157 y-161)) (x-166 (MOD z-162 26)) (x-168 (ADD x-166 13)) (x-169 (EQL x-168 j)) (x-170 (EQL x-169 0)) (y-178 (ADD j 6)) (y-179 (MUL y-178 x-170)) (y-173 (MUL 25 x-170)) (y-174 (ADD y-173 1)) (z-175 (MUL z-162 y-174)) (z-180 (ADD z-175 y-179)) (x-184 (MOD z-180 26)) (x-186 (ADD x-184 -12)) (x-187 (EQL x-186 k)) (x-188 (EQL x-187 0)) (y-196 (ADD k 8)) (y-197 (MUL y-196 x-188)) (y-191 (MUL 25 x-188)) (y-192 (ADD y-191 1)) (z-185 (DIV z-180 26)) (z-193 (MUL z-185 y-192)) (z-198 (ADD z-193 y-197)) (x-202 (MOD z-198 26)) (x-204 (ADD x-202 -3)) (x-205 (EQL x-204 l)) (x-206 (EQL x-205 0)) (y-214 (ADD l 11)) (y-215 (MUL y-214 x-206)) (y-209 (MUL 25 x-206)) (y-210 (ADD y-209 1)) (z-203 (DIV z-198 26)) (z-211 (MUL z-203 y-210)) (z-216 (ADD z-211 y-215)) (x-220 (MOD z-216 26)) (x-222 (ADD x-220 -11)) (x-223 (EQL x-222 m)) (x-224 (EQL x-223 0)) (y-232 (ADD m 5)) (y-233 (MUL y-232 x-224)) (y-227 (MUL 25 x-224)) (y-228 (ADD y-227 1)) (z-221 (DIV z-216 26)) (z-229 (MUL z-221 y-228)) (z-234 (ADD z-229 y-233)) (x-238 (MOD z-234 26)) (x-240 (ADD x-238 -2)) (x-241 (EQL x-240 n)) (x-242 (EQL x-241 0)) (y-250 (ADD n 11)) (y-251 (MUL y-250 x-242)) (y-245 (MUL 25 x-242)) (y-246 (ADD y-245 1)) (z-239 (DIV z-234 26)) (z-247 (MUL z-239 y-246)) (z-252 (ADD z-247 y-251))))
         (z 'z-252))
;;(let*-values  (((ordered-bindings z) (program->ordered-bindings program)))
     (ordered-bindings->scheme-proc ordered-bindings z)))

(define (ordered-bindings->scheme-proc ordered-bindings z)
   (let*-values (
                 ((equal-for-MONAD) (lambda (a b) (if (= a b) 1 0)))
                 ((scheme-expression)
                  `(lambda (a b c d e f g h i j k l m n)
                     (let ((ADD ,+)
                           (MUL ,*)
                           (DIV ,quotient)
                           (MOD ,remainder)
                           (EQL ,equal-for-MONAD))
                        (let* ,ordered-bindings
                          ,z)))))
      (compile scheme-expression #:env (interaction-environment))))

(define (program->optimised-scheme-proc program)
  (let* ((ordered-bindings '((x-7 (EQL 10 a)) (x-8 (EQL x-7 0)) (y-16 (ADD a 2)) (y-17 (MUL y-16 x-8)) (x-22 (MOD y-17 26)) (x-24 (ADD x-22 10)) (x-25 (EQL x-24 b)) (x-26 (EQL x-25 0)) (y-34 (ADD b 4)) (y-35 (MUL y-34 x-26)) (y-29 (MUL 25 x-26)) (y-30 (ADD y-29 1)) (z-31 (MUL y-17 y-30)) (z-36 (ADD z-31 y-35)) (x-40 (MOD z-36 26)) (x-42 (ADD x-40 14)) (x-43 (EQL x-42 c)) (x-44 (EQL x-43 0)) (y-52 (ADD c 8)) (y-53 (MUL y-52 x-44)) (y-47 (MUL 25 x-44)) (y-48 (ADD y-47 1)) (z-49 (MUL z-36 y-48)) (z-54 (ADD z-49 y-53)) (x-58 (MOD z-54 26)) (x-60 (ADD x-58 11)) (x-61 (EQL x-60 d)) (x-62 (EQL x-61 0)) (y-70 (ADD d 7)) (y-71 (MUL y-70 x-62)) (y-65 (MUL 25 x-62)) (y-66 (ADD y-65 1)) (z-67 (MUL z-54 y-66)) (z-72 (ADD z-67 y-71)) (x-76 (MOD z-72 26)) (x-78 (ADD x-76 14)) (x-79 (EQL x-78 e)) (x-80 (EQL x-79 0)) (y-88 (ADD e 12)) (y-89 (MUL y-88 x-80)) (y-83 (MUL 25 x-80)) (y-84 (ADD y-83 1)) (z-85 (MUL z-72 y-84)) (z-90 (ADD z-85 y-89)) (x-94 (MOD z-90 26)) (x-96 (ADD x-94 -14)) (x-97 (EQL x-96 f)) (x-98 (EQL x-97 0)) (y-106 (ADD f 7)) (y-107 (MUL y-106 x-98)) (y-101 (MUL 25 x-98)) (y-102 (ADD y-101 1)) (z-95 (DIV z-90 26)) (z-103 (MUL z-95 y-102)) (z-108 (ADD z-103 y-107)) (x-112 (MOD z-108 26)) (x-115 (EQL x-112 g)) (x-116 (EQL x-115 0)) (y-124 (ADD g 10)) (y-125 (MUL y-124 x-116)) (y-119 (MUL 25 x-116)) (y-120 (ADD y-119 1)) (z-113 (DIV z-108 26)) (z-121 (MUL z-113 y-120)) (z-126 (ADD z-121 y-125)) (x-130 (MOD z-126 26)) (x-132 (ADD x-130 10)) (x-133 (EQL x-132 h)) (x-134 (EQL x-133 0)) (y-142 (ADD h 14)) (y-143 (MUL y-142 x-134)) (y-137 (MUL 25 x-134)) (y-138 (ADD y-137 1)) (z-139 (MUL z-126 y-138)) (z-144 (ADD z-139 y-143)) (x-148 (MOD z-144 26)) (x-150 (ADD x-148 -10)) (x-151 (EQL x-150 i)) (x-152 (EQL x-151 0)) (y-160 (ADD i 2)) (y-161 (MUL y-160 x-152)) (y-155 (MUL 25 x-152)) (y-156 (ADD y-155 1)) (z-149 (DIV z-144 26)) (z-157 (MUL z-149 y-156)) (z-162 (ADD z-157 y-161)) (x-166 (MOD z-162 26)) (x-168 (ADD x-166 13)) (x-169 (EQL x-168 j)) (x-170 (EQL x-169 0)) (y-178 (ADD j 6)) (y-179 (MUL y-178 x-170)) (y-173 (MUL 25 x-170)) (y-174 (ADD y-173 1)) (z-175 (MUL z-162 y-174)) (z-180 (ADD z-175 y-179)) (x-184 (MOD z-180 26)) (x-186 (ADD x-184 -12)) (x-187 (EQL x-186 k)) (x-188 (EQL x-187 0)) (y-196 (ADD k 8)) (y-197 (MUL y-196 x-188)) (y-191 (MUL 25 x-188)) (y-192 (ADD y-191 1)) (z-185 (DIV z-180 26)) (z-193 (MUL z-185 y-192)) (z-198 (ADD z-193 y-197)) (x-202 (MOD z-198 26)) (x-204 (ADD x-202 -3)) (x-205 (EQL x-204 l)) (x-206 (EQL x-205 0)) (y-214 (ADD l 11)) (y-215 (MUL y-214 x-206)) (y-209 (MUL 25 x-206)) (y-210 (ADD y-209 1)) (z-203 (DIV z-198 26)) (z-211 (MUL z-203 y-210)) (z-216 (ADD z-211 y-215)) (x-220 (MOD z-216 26)) (x-222 (ADD x-220 -11)) (x-223 (EQL x-222 m)) (x-224 (EQL x-223 0)) (y-232 (ADD m 5)) (y-233 (MUL y-232 x-224)) (y-227 (MUL 25 x-224)) (y-228 (ADD y-227 1)) (z-221 (DIV z-216 26)) (z-229 (MUL z-221 y-228)) (z-234 (ADD z-229 y-233)) (x-238 (MOD z-234 26)) (x-240 (ADD x-238 -2)) (x-241 (EQL x-240 n)) (x-242 (EQL x-241 0)) (y-250 (ADD n 11)) (y-251 (MUL y-250 x-242)) (y-245 (MUL 25 x-242)) (y-246 (ADD y-245 1)) (z-239 (DIV z-234 26)) (z-247 (MUL z-239 y-246)) (z-252 (ADD z-247 y-251))))
         (z 'z-252))
;;(let*-values  (((ordered-bindings z) (program->ordered-bindings program)))
     (ordered-bindings->optimised-scheme-proc ordered-bindings z)))

(define (ordered-bindings->optimised-scheme-proc ordered-bindings z)
  (let*-values  (
                 (_ (dbg "bnds-raw__=" ordered-bindings))
                 ((reversed-bindings) (range-analysis ordered-bindings))
                 ((ordered-bindings) (prune-bindings-reverse reversed-bindings z))
                 (_ (dbg "bnds-optim=" ordered-bindings))
                 ((equal-for-MONAD) (lambda (a b) (if (= a b) 1 0)))
                 ((scheme-expression)
                  `(lambda (a b c d e f g h i j k l m n)
                     (let ((ADD ,+)
                           (MUL ,*)
                           (DIV ,quotient)
                           (MOD ,remainder)
                           (EQL ,equal-for-MONAD))
                        (let* ,ordered-bindings
                          ,z)))))
      (dbg "scheme-expr=" scheme-expression)
      (eval scheme-expression (interaction-environment))))

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

(define (highest-fourteen-digit-validated proc from)
   (let highest-rec ((num from))
     (if (= 0 (remainder num 1000000))
         (dbg "num=" num))
     (let* ((num-as-list (num->monad-input num))
            (valid? (if num-as-list
                        (= 0 (apply proc num-as-list))
                        #f)))
        (cond
         (valid? num)
         (else (highest-rec (1- num)))))))

(define (highest-digits-rec programs registers depth)
  (cond
   ((null? programs)
    (if (= 0 (reg-z registers))
        1
        #f))
   (else
     (let loop ((digits (iota 9 9 -1)))
       (if (< depth 8)
           (dbg "depth,digits=" (list depth digits)))
       (cond
        ((null? digits) #f)
        (else (let* ((program (car programs))
                     (digit (car digits))
                     (newstate (run-from-registers program (list digit) registers))
                     (subresult (highest-digits-rec (cdr programs) (state-regs newstate) (1+ depth)))
                     (result (if subresult (+ digit (* 10 subresult))
                                           (loop (cdr digits)))))
                 result)))))))

(define (highest-digits programs)
    (highest-digits-rec programs empty-registers 0))

(use-modules (statprof))
(define (compiled-program a b c d e f g h i j k l m n)
    (let  (
           (ADD +)
           (MUL *)
           (DIV quotient)
           (MOD remainder)
           (EQL (lambda (a b) (if (= a b) 1 0))))
        (let* 
            (
             (x-7    (EQL 10 a)) 
             (x-8    (EQL x-7 0)) 
             (y-16   (ADD a 2)) 
             (y-17   (MUL y-16 x-8)) 
             (x-22   (MOD y-17 26)) 
             (x-24   (ADD x-22 10)) 
             (x-25   (EQL x-24 b)) 
             (x-26   (EQL x-25 0)) 
             (y-34   (ADD b 4)) 
             (y-35   (MUL y-34 x-26)) 
             (y-29   (MUL 25 x-26)) 
             (y-30   (ADD y-29 1)) 
             (z-31   (MUL y-17 y-30)) 
             (z-36   (ADD z-31 y-35)) 
             (x-40   (MOD z-36 26)) 
             (x-42   (ADD x-40 14)) 
             (x-43   (EQL x-42 c)) 
             (x-44   (EQL x-43 0))
             (y-52   (ADD c 8))
             (y-53   (MUL y-52 x-44))
             (y-47   (MUL 25 x-44))
             (y-48   (ADD y-47 1))
             (z-49   (MUL z-36 y-48))
             (z-54   (ADD z-49 y-53))
             (x-58   (MOD z-54 26))
             (x-60   (ADD x-58 11))
             (x-61   (EQL x-60 d))
             (x-62   (EQL x-61 0))
             (y-70   (ADD d 7))
             (y-71   (MUL y-70 x-62))
             (y-65   (MUL 25 x-62))
             (y-66   (ADD y-65 1))
             (z-67   (MUL z-54 y-66))
             (z-72   (ADD z-67 y-71))
             (x-76   (MOD z-72 26))
             (x-78   (ADD x-76 14))
             (x-79   (EQL x-78 e))
             (x-80   (EQL x-79 0))
             (y-88   (ADD e 12))
             (y-89   (MUL y-88 x-80))
             (y-83   (MUL 25 x-80))
             (y-84   (ADD y-83 1))
             (z-85   (MUL z-72 y-84))
             (z-90   (ADD z-85 y-89))
             (x-94   (MOD z-90 26))
             (x-96   (ADD x-94 -14))
             (x-97   (EQL x-96 f))
             (x-98   (EQL x-97 0))
             (y-106  (ADD f 7)) 
             (y-107  (MUL y-106 x-98)) 
             (y-101  (MUL 25 x-98)) 
             (y-102  (ADD y-101 1)) 
             (z-95   (DIV z-90 26))
             (z-103  (MUL z-95 y-102)) 
             (z-108  (ADD z-103 y-107)) 
             (x-112  (MOD z-108 26)) 
             (x-115  (EQL x-112 g)) 
             (x-116  (EQL x-115 0)) 
             (y-124  (ADD g 10)) 
             (y-125  (MUL y-124 x-116)) 
             (y-119  (MUL 25 x-116)) 
             (y-120  (ADD y-119 1)) 
             (z-113  (DIV z-108 26)) 
             (z-121  (MUL z-113 y-120)) 
             (z-126  (ADD z-121 y-125)) 
             (x-130  (MOD z-126 26)) 
             (x-132  (ADD x-130 10)) 
             (x-133  (EQL x-132 h)) 
             (x-134  (EQL x-133 0)) 
             (y-142  (ADD h 14)) 
             (y-143  (MUL y-142 x-134)) 
             (y-137  (MUL 25 x-134)) 
             (y-138  (ADD y-137 1)) 
             (z-139  (MUL z-126 y-138)) 
             (z-144  (ADD z-139 y-143)) 
             (x-148  (MOD z-144 26)) 
             (x-150  (ADD x-148 -10)) 
             (x-151  (EQL x-150 i)) 
             (x-152  (EQL x-151 0)) 
             (y-160  (ADD i 2)) 
             (y-161  (MUL y-160 x-152)) 
             (y-155  (MUL 25 x-152)) 
             (y-156  (ADD y-155 1)) 
             (z-149  (DIV z-144 26)) 
             (z-157  (MUL z-149 y-156)) 
             (z-162  (ADD z-157 y-161)) 
             (x-166  (MOD z-162 26)) 
             (x-168  (ADD x-166 13)) 
             (x-169  (EQL x-168 j)) 
             (x-170  (EQL x-169 0)) 
             (y-178  (ADD j 6)) 
             (y-179  (MUL y-178 x-170)) 
             (y-173  (MUL 25 x-170)) 
             (y-174  (ADD y-173 1)) 
             (z-175  (MUL z-162 y-174)) 
             (z-180  (ADD z-175 y-179)) 
             (x-184  (MOD z-180 26)) 
             (x-186  (ADD x-184 -12)) 
             (x-187  (EQL x-186 k)) 
             (x-188  (EQL x-187 0)) 
             (y-196  (ADD k 8)) 
             (y-197  (MUL y-196 x-188)) 
             (y-191  (MUL 25 x-188)) 
             (y-192  (ADD y-191 1)) 
             (z-185  (DIV z-180 26)) 
             (z-193  (MUL z-185 y-192)) 
             (z-198  (ADD z-193 y-197)) 
             (x-202  (MOD z-198 26)) 
             (x-204  (ADD x-202 -3)) 
             (x-205  (EQL x-204 l)) 
             (x-206  (EQL x-205 0)) 
             (y-214  (ADD l 11)) 
             (y-215  (MUL y-214 x-206)) 
             (y-209  (MUL 25 x-206)) 
             (y-210  (ADD y-209 1)) 
             (z-203  (DIV z-198 26)) 
             (z-211  (MUL z-203 y-210)) 
             (z-216  (ADD z-211 y-215)) 
             (x-220  (MOD z-216 26)) 
             (x-222  (ADD x-220 -11)) 
             (x-223  (EQL x-222 m)) 
             (x-224  (EQL x-223 0)) 
             (y-232  (ADD m 5)) 
             (y-233  (MUL y-232 x-224)) 
             (y-227  (MUL 25 x-224)) 
             (y-228  (ADD y-227 1)) 
             (z-221  (DIV z-216 26)) 
             (z-229  (MUL z-221 y-228)) 
             (z-234  (ADD z-229 y-233)) 
             (x-238  (MOD z-234 26)) 
             (x-240  (ADD x-238 -2)) 
             (x-241  (EQL x-240 n)) 
             (x-242  (EQL x-241 0)) 
             (y-250  (ADD n 11)) 
             (y-251  (MUL y-250 x-242)) 
             (y-245  (MUL 25 x-242)) 
             (y-246  (ADD y-245 1)) 
             (z-239  (DIV z-234 26)) 
             (z-247  (MUL z-239 y-246)) 
             (z-252  (ADD z-247 y-251)))
           z-252)))

(define-public (main args)
   (let*-values (
                 ((programs) (parse-programs (current-input-port)))
                 ;(_ (statprof (lambda () (highest-fourtenn-digit-validated program (+ 5000000 (expt 10 13))))))
                 ;((highest) (highest-fourteen-digit-validated optimised-program (1- (expt 10 14))))
                 ((result1) (highest-digits programs))
                 ((result2) "UNIMP")) ;(run program '(1 2 3 4 5 6 7 8 9 9 1 2 3 4 5))))
      (format #t "result1: ~a\n" result1)
      (format #t "result2: ~a\n" result2)))

;; ;;(lambda (a b c d e f g h i j k l m n) (let ((ADD #<procedure + (#:optional _ _ . _)>) (MUL #<procedure * (#:optional _ _ . _)>) (DIV #<procedure quotient (_ _)>) (MOD #<procedure remainder (_ _)>) (EQL #<procedure = (#:optional _ _ . _)>)) (let* ((x-7 (EQL 10 a)) (x-8 (EQL x-7 0)) (y-16 (ADD a 2)) (y-17 (MUL y-16 x-8)) (x-22 (MOD y-17 26)) (x-24 (ADD x-22 10)) (x-25 (EQL x-24 b)) (x-26 (EQL x-25 0)) (y-34 (ADD b 4)) (y-35 (MUL y-34 x-26)) (y-29 (MUL 25 x-26)) (y-30 (ADD y-29 1)) (z-31 (MUL y-17 y-30)) (z-36 (ADD z-31 y-35)) (x-40 (MOD z-36 26)) (x-42 (ADD x-40 14)) (x-43 (EQL x-42 c)) (x-44 (EQL x-43 0)) (y-52 (ADD c 8)) (y-53 (MUL y-52 x-44)) (y-47 (MUL 25 x-44)) (y-48 (ADD y-47 1)) (z-49 (MUL z-36 y-48)) (z-54 (ADD z-49 y-53)) (x-58 (MOD z-54 26)) (x-60 (ADD x-58 11)) (x-61 (EQL x-60 d)) (x-62 (EQL x-61 0)) (y-70 (ADD d 7)) (y-71 (MUL y-70 x-62)) (y-65 (MUL 25 x-62)) (y-66 (ADD y-65 1)) (z-67 (MUL z-54 y-66)) (z-72 (ADD z-67 y-71)) (x-76 (MOD z-72 26)) (x-78 (ADD x-76 14)) (x-79 (EQL x-78 e)) (x-80 (EQL x-79 0)) (y-88 (ADD e 12)) (y-89 (MUL y-88 x-80)) (y-83 (MUL 25 x-80)) (y-84 (ADD y-83 1)) (z-85 (MUL z-72 y-84)) (z-90 (ADD z-85 y-89)) (x-94 (MOD z-90 26)) (x-96 (ADD x-94 -14)) (x-97 (EQL x-96 f)) (x-98 (EQL x-97 0)) (y-106 (ADD f 7)) (y-107 (MUL y-106 x-98)) (y-101 (MUL 25 x-98)) (y-102 (ADD y-101 1)) (z-95 (DIV z-90 26)) (z-103 (MUL z-95 y-102)) (z-108 (ADD z-103 y-107)) (x-112 (MOD z-108 26)) (x-115 (EQL x-112 g)) (x-116 (EQL x-115 0)) (y-124 (ADD g 10)) (y-125 (MUL y-124 x-116)) (y-119 (MUL 25 x-116)) (y-120 (ADD y-119 1)) (z-113 (DIV z-108 26)) (z-121 (MUL z-113 y-120)) (z-126 (ADD z-121 y-125)) (x-130 (MOD z-126 26)) (x-132 (ADD x-130 10)) (x-133 (EQL x-132 h)) (x-134 (EQL x-133 0)) (y-142 (ADD h 14)) (y-143 (MUL y-142 x-134)) (y-137 (MUL 25 x-134)) (y-138 (ADD y-137 1)) (z-139 (MUL z-126 y-138)) (z-144 (ADD z-139 y-143)) (x-148 (MOD z-144 26)) (x-150 (ADD x-148 -10)) (x-151 (EQL x-150 i)) (x-152 (EQL x-151 0)) (y-160 (ADD i 2)) (y-161 (MUL y-160 x-152)) (y-155 (MUL 25 x-152)) (y-156 (ADD y-155 1)) (z-149 (DIV z-144 26)) (z-157 (MUL z-149 y-156)) (z-162 (ADD z-157 y-161)) (x-166 (MOD z-162 26)) (x-168 (ADD x-166 13)) (x-169 (EQL x-168 j)) (x-170 (EQL x-169 0)) (y-178 (ADD j 6)) (y-179 (MUL y-178 x-170)) (y-173 (MUL 25 x-170)) (y-174 (ADD y-173 1)) (z-175 (MUL z-162 y-174)) (z-180 (ADD z-175 y-179)) (x-184 (MOD z-180 26)) (x-186 (ADD x-184 -12)) (x-187 (EQL x-186 k)) (x-188 (EQL x-187 0)) (y-196 (ADD k 8)) (y-197 (MUL y-196 x-188)) (y-191 (MUL 25 x-188)) (y-192 (ADD y-191 1)) (z-185 (DIV z-180 26)) (z-193 (MUL z-185 y-192)) (z-198 (ADD z-193 y-197)) (x-202 (MOD z-198 26)) (x-204 (ADD x-202 -3)) (x-205 (EQL x-204 l)) (x-206 (EQL x-205 0)) (y-214 (ADD l 11)) (y-215 (MUL y-214 x-206)) (y-209 (MUL 25 x-206)) (y-210 (ADD y-209 1)) (z-203 (DIV z-198 26)) (z-211 (MUL z-203 y-210)) (z-216 (ADD z-211 y-215)) (x-220 (MOD z-216 26)) (x-222 (ADD x-220 -11)) (x-223 (EQL x-222 m)) (x-224 (EQL x-223 0)) (y-232 (ADD m 5)) (y-233 (MUL y-232 x-224)) (y-227 (MUL 25 x-224)) (y-228 (ADD y-227 1)) (z-221 (DIV z-216 26)) (z-229 (MUL z-221 y-228)) (z-234 (ADD z-229 y-233)) (x-238 (MOD z-234 26)) (x-240 (ADD x-238 -2)) (x-241 (EQL x-240 n)) (x-242 (EQL x-241 0)) (y-250 (ADD n 11)) (y-251 (MUL y-250 x-242)) (y-245 (MUL 25 x-242)) (y-246 (ADD y-245 1)) (z-239 (DIV z-234 26)) (z-247 (MUL z-239 y-246)) (z-252 (ADD z-247 y-251))) z-252)))
;; 
;; 
;; (define pg (parse-program (open-input-file "inputs/input")))
;; (define symbolic (symbolic-analysis pg))
;; (define-values (bnd z) (program->ordered-bindings pg))
;; ;(define-values (bnd z) (values '((x-7 (EQL 10 a)) (x-8 (EQL x-7 0)) (y-16 (ADD a 2)) (y-17 (MUL y-16 x-8)) (x-22 (MOD y-17 26)) (x-24 (ADD x-22 10)) (x-25 (EQL x-24 b)) (x-26 (EQL x-25 0)) (y-34 (ADD b 4)) (y-35 (MUL y-34 x-26)) (y-29 (MUL 25 x-26)) (y-30 (ADD y-29 1)) (z-31 (MUL y-17 y-30)) (z-36 (ADD z-31 y-35)) (x-40 (MOD z-36 26)) (x-42 (ADD x-40 14)) (x-43 (EQL x-42 c)) (x-44 (EQL x-43 0)) (y-52 (ADD c 8)) (y-53 (MUL y-52 x-44)) (y-47 (MUL 25 x-44)) (y-48 (ADD y-47 1)) (z-49 (MUL z-36 y-48)) (z-54 (ADD z-49 y-53)) (x-58 (MOD z-54 26)) (x-60 (ADD x-58 11)) (x-61 (EQL x-60 d)) (x-62 (EQL x-61 0)) (y-70 (ADD d 7)) (y-71 (MUL y-70 x-62)) (y-65 (MUL 25 x-62)) (y-66 (ADD y-65 1)) (z-67 (MUL z-54 y-66)) (z-72 (ADD z-67 y-71)) (x-76 (MOD z-72 26)) (x-78 (ADD x-76 14)) (x-79 (EQL x-78 e)) (x-80 (EQL x-79 0)) (y-88 (ADD e 12)) (y-89 (MUL y-88 x-80)) (y-83 (MUL 25 x-80)) (y-84 (ADD y-83 1)) (z-85 (MUL z-72 y-84)) (z-90 (ADD z-85 y-89)) (x-94 (MOD z-90 26)) (x-96 (ADD x-94 -14)) (x-97 (EQL x-96 f)) (x-98 (EQL x-97 0)) (y-106 (ADD f 7)) (y-107 (MUL y-106 x-98)) (y-101 (MUL 25 x-98)) (y-102 (ADD y-101 1)) (z-95 (DIV z-90 26)) (z-103 (MUL z-95 y-102)) (z-108 (ADD z-103 y-107)) (x-112 (MOD z-108 26)) (x-115 (EQL x-112 g)) (x-116 (EQL x-115 0)) (y-124 (ADD g 10)) (y-125 (MUL y-124 x-116)) (y-119 (MUL 25 x-116)) (y-120 (ADD y-119 1)) (z-113 (DIV z-108 26)) (z-121 (MUL z-113 y-120)) (z-126 (ADD z-121 y-125)) (x-130 (MOD z-126 26)) (x-132 (ADD x-130 10)) (x-133 (EQL x-132 h)) (x-134 (EQL x-133 0)) (y-142 (ADD h 14)) (y-143 (MUL y-142 x-134)) (y-137 (MUL 25 x-134)) (y-138 (ADD y-137 1)) (z-139 (MUL z-126 y-138)) (z-144 (ADD z-139 y-143)) (x-148 (MOD z-144 26)) (x-150 (ADD x-148 -10)) (x-151 (EQL x-150 i)) (x-152 (EQL x-151 0)) (y-160 (ADD i 2)) (y-161 (MUL y-160 x-152)) (y-155 (MUL 25 x-152)) (y-156 (ADD y-155 1)) (z-149 (DIV z-144 26)) (z-157 (MUL z-149 y-156)) (z-162 (ADD z-157 y-161)) (x-166 (MOD z-162 26)) (x-168 (ADD x-166 13)) (x-169 (EQL x-168 j)) (x-170 (EQL x-169 0)) (y-178 (ADD j 6)) (y-179 (MUL y-178 x-170)) (y-173 (MUL 25 x-170)) (y-174 (ADD y-173 1)) (z-175 (MUL z-162 y-174)) (z-180 (ADD z-175 y-179)) (x-184 (MOD z-180 26)) (x-186 (ADD x-184 -12)) (x-187 (EQL x-186 k)) (x-188 (EQL x-187 0)) (y-196 (ADD k 8)) (y-197 (MUL y-196 x-188)) (y-191 (MUL 25 x-188)) (y-192 (ADD y-191 1)) (z-185 (DIV z-180 26)) (z-193 (MUL z-185 y-192)) (z-198 (ADD z-193 y-197)) (x-202 (MOD z-198 26)) (x-204 (ADD x-202 -3)) (x-205 (EQL x-204 l)) (x-206 (EQL x-205 0)) (y-214 (ADD l 11)) (y-215 (MUL y-214 x-206)) (y-209 (MUL 25 x-206)) (y-210 (ADD y-209 1)) (z-203 (DIV z-198 26)) (z-211 (MUL z-203 y-210)) (z-216 (ADD z-211 y-215)) (x-220 (MOD z-216 26)) (x-222 (ADD x-220 -11)) (x-223 (EQL x-222 m)) (x-224 (EQL x-223 0)) (y-232 (ADD m 5)) (y-233 (MUL y-232 x-224)) (y-227 (MUL 25 x-224)) (y-228 (ADD y-227 1)) (z-221 (DIV z-216 26)) (z-229 (MUL z-221 y-228)) (z-234 (ADD z-229 y-233)) (x-238 (MOD z-234 26)) (x-240 (ADD x-238 -2)) (x-241 (EQL x-240 n)) (x-242 (EQL x-241 0)) (y-250 (ADD n 11)) (y-251 (MUL y-250 x-242)) (y-245 (MUL 25 x-242)) (y-246 (ADD y-245 1)) (z-239 (DIV z-234 26)) (z-247 (MUL z-239 y-246)) (z-252 (ADD z-247 y-251))) 'z252))
;; (define bnd-anl (range-analysis bnd))
;; (define bnd-anl-prn (prune-bindings-reverse bnd-anl 'z-252))
;; (for-each
;;  (lambda (x)
;;      (format #t "~a\n" x))
;;  bnd-anl)
;; 
