(define-module (second)
    #:use-module (ice-9 rdelim)
    #:use-module (ice-9 match)
    #:use-module (ice-9 format)
    #:use-module (srfi srfi-1) ; lists library
    #:use-module (srfi srfi-9)
    #:use-module (srfi srfi-9 gnu) ; immutable records
    #:use-module (srfi srfi-41))

(define (dbg t v) (format #t "~s: ~a\n" t v))

(define stream-of-lines
  (stream-lambda ()
     (let
         ((line (read-line)))
      (cond
       ((eof-object? line) stream-null)
       (else (stream-cons line (stream-of-lines)))))))

(define (o2-generator-rating lst)
   (define (rec-o2-generator-rating lst pos)
       (cond
        ((eq? (length lst) 1) (car lst))
        (else
         (rec-o2-generator-rating (cdr lst)))))
   (rec-o2-generator-rating lst 1))

(define (most-common lst)
   (define (rec-most-common lst zeros ones)
     (cond
      ((eq? lst '()) (if (>= ones zeros) 1 0))
      (else
       (let* ((num (car lst))
              (zeros (if (eq? num 0) (+ 1 zeros) zeros))
              (ones (if (eq? num 1) (+ 1 ones) ones)))
         (rec-most-common (cdr lst) zeros ones)))))
   (rec-most-common lst 0 0))

(define-public (main args)
   (format #t "result is: ~d" 0))
