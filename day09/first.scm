(define-module (first)
    #:use-module (ice-9 rdelim)
    #:use-module (ice-9 match)
    #:use-module (ice-9 format)
    #:use-module (srfi srfi-1) ; lists library
    #:use-module (srfi srfi-11) ; let*-values
    #:use-module (srfi srfi-9)
    #:use-module (srfi srfi-9 gnu) ; immutable records
    #:use-module (srfi srfi-41) ; Streams
    #:use-module (srfi srfi-43)) ; Vectors iterators

(use-modules (ice-9 rdelim))
(use-modules (ice-9 match))
(use-modules (ice-9 format))
(use-modules (srfi srfi-1)) ; lists library
(use-modules (srfi srfi-11)) ; let*-values
(use-modules (srfi srfi-9))
(use-modules (srfi srfi-9 gnu)) ; immutable records
(use-modules (srfi srfi-41))

(define (dbg t v) (format #t "~s: ~a\n" t v) (force-output))

(define stream-of-lines
  (stream-lambda (port)
     (let
         ((line (read-line port)))
      (cond
       ((eof-object? line) stream-null)
       (else (stream-cons line (stream-of-lines port)))))))

(define (parse-line line)
   (let* (
          (x (string->list line))
          (x (map char->integer x))
          (x (map (lambda (v) (- v (char->integer #\0))) x)))
     x))

(define (stream-of-lines->array strm)
   (let* ((x (stream-map parse-line strm))
          (x (stream->list x))
          (x (list->array 2 x)))
     x))

(define (neighbours l c maxl maxc)
  (filter
     (lambda (x) (begin
                  (and x (not (equal? x (cons l c))))))
     (append-map
       (lambda (ld)
          (let ((lres (- l ld)))
            (if (and (>= lres 0) (< lres maxl))
                (map (lambda (cd)
                        (let ((cres (- c cd)))
                           (if (and (>= cres 0) (< cres maxc))
                               (cons lres cres)
                               #f)))
                     '(-1 0 1))
                '())))
       '(-1 0 1))))


;;; (define (lower a l c)
;;;    (fold
;;;       (lambda (cur acc)
;;;          (let ((curval (array-ref a (car cur) (cdr cur))))
;;;            (if (not acc)
;;;                curval
;;;                (min curval acc))))
;;;     #f
;;;     (let* ((dims (array-dimensions a))
;;;            (maxl (car dims))
;;;            (maxc (cadr dims)))
;;;        (dbg "l" l)
;;;        (dbg "c" c)
;;;        (dbg "maxl" maxl)
;;;        (dbg "maxc" maxc)
;;;        (neighbours l c maxl maxc))))

(define (low? a l c)
  (let* ((dims (array-dimensions a))
         (maxl (car dims))
         (maxc (cadr dims))
         (thisval (array-ref a l c)))
     (let loop ((neighbours (neighbours l c maxl maxc)))
            (if (eqv? '() neighbours)
                #t
                (let* ((curpos (car neighbours))
                       (curval (array-ref a (car curpos) (cdr curpos))))
                   (if (<= curval thisval)
                       #f
                       (loop (cdr neighbours))))))))

(define (array-for-each-index a proc-cell proc-endofline)
  (let* ((dims (array-dimensions a))
         (maxl (car dims))
         (maxc (cadr dims)))
    (do ((i 0 (1+ i)))
        ((>= i maxl))
      (do ((j 0 (1+ j)))
          ((>= j maxc))
        (proc-cell i j (array-ref a i j)))
      (proc-endofline i))))

(define (print-array a)
  (array-for-each-index
       a
       (lambda (i j v) (display v))
       (lambda (i) (display "\n"))))

(define (all-lows a)
  (let* ((dims (array-dimensions a))
         (maxl (car dims))
         (maxc (cadr dims))
         (out (make-array 'u maxl maxc)))
    (array-for-each-index
         a
         (lambda (i j v) (array-set! out (if (low? a i j) 'x 'n) i j))
         (lambda (i) #f))
    out))

(define (risk-level a)
  (let* ((dims (array-dimensions a))
         (maxl (car dims))
         (maxc (cadr dims))
         (out 0))
    (array-for-each-index
         a
         (lambda (i j v) (set! out (if (low? a i j) (+ out 1 v) out)))
         (lambda (i) #f))
    out))

(define-public (main args)
  (let* (
         (strm (stream-of-lines (current-input-port)))
         (a (stream-of-lines->array strm))
         (result1 (risk-level a))
         (result2 "UNIMP"))
    (format #t "result1: ~a\n" result1)
    (format #t "result2: ~a\n" result2)))
