(define-module (first)
    #:use-module (ice-9 rdelim)
    #:use-module (ice-9 match)
    #:use-module (srfi srfi-1) ; lists library
    #:use-module (srfi srfi-9)
    #:use-module (srfi srfi-9 gnu) ; immutable records
    #:use-module (srfi srfi-41))

(define stream-of-lines
  (stream-lambda ()
     (let
         ((line (read-line)))
      (cond
       ((eof-object? line) stream-null)
       (else (stream-cons line (stream-of-lines)))))))

(define-immutable-record-type <binary-counter>
    (make-binary-counter zeros ones)
    binary-counter?
    (zeros count0 set-count0)
    (ones count1 set-count1))

(define binary-counter-empty (make-binary-counter 0 0))

(define (inc0 counter)
    (set-count0
     counter
     (+ 1 (count0 counter))))

(define (inc1 counter)
    (set-count1
     counter
     (+ 1 (count1 counter))))

(define (add-char-to-counter counter c)
    (cond
     ((char=? #\0 c) (inc0 counter))
     ((char=? #\1 c) (inc1 counter))))

(define list-of-5-binary-counter-empty
    (list
     binary-counter-empty
     binary-counter-empty
     binary-counter-empty
     binary-counter-empty
     binary-counter-empty))

(define (add-to-list-of-binary-counter acc cur)
    (map add-char-to-counter acc cur))

(define (count-binary-in-diagnostic-stream strm)
   (stream-fold
    add-to-list-of-binary-counter
    list-of-5-binary-counter-empty
    strm))

(define (min-max-from-counter counter)
    (let* ((zeros (count0 counter))
           (ones (count1 counter)))
     (cond
      ((< zeros ones) (cons 0 1))
      (else (cons 1 0)))))

(define (dbg t v) (format #t "~s: ~a\n" t v))
(define (epsilon-and-gamma lst)
    (let* ((x (map min-max-from-counter lst))
           (x (fold
                     (lambda (cur exponent acc)
                       (cons
                         (+ (car acc) (* (car cur) exponent))
                         (+ (cdr acc) (* (cdr cur) exponent))))
                     '(0 . 0) ; initial acc(umulator)
                     x ; list of binary digits
                     '(16 8 4 2 1)))) ; exponents
        x))

(define (gamma lst)
    (cdr (epsilon-and-gamma lst)))

(define (epsilon lst)
    (car (epsilon-and-gamma lst)))

(define (get-power-consumption)
    ;; Power consumption is produce of epsilon*gamma
    ;; We extract epsilon and gamma from diag text in current-input-port
    (let* ((x (stream-of-lines))
           (x (stream-map string->list x))
           (x (count-binary-in-diagnostic-stream x))
           (x (epsilon-and-gamma x)))
      (* (car x) (cdr x))))


(define-public (main args)
   (format #t "result is: ~d" (get-power-consumption)))
