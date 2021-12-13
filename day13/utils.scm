(define-module (adventofcode2021 day13 utils)
    #:use-module (ice-9 rdelim) ; read-line
    #:use-module (ice-9 match)
    #:use-module (ice-9 format)
    #:use-module (srfi srfi-1) ; lists library
    #:use-module (srfi srfi-2) ; and-let*
    #:use-module (srfi srfi-11) ; let*-values
    #:use-module (srfi srfi-9)
    #:use-module (srfi srfi-9 gnu) ; immutable records
    #:use-module (srfi srfi-41) ; Streams
    #:use-module (srfi srfi-43)) ; Vectors iterators

(use-modules (ice-9 rdelim))
(define-public (dbg t v) (format #t "~s: ~a\n" t v) (force-output))

(define-public stream-of-lines
  (stream-lambda (port)
     (let
         ((line (read-line port)))
      (cond
       ((eof-object? line) stream-null)
       (else (stream-cons line (stream-of-lines port)))))))

(define-public (read-bloc port)
  (let loop ((line (read-line port)) (out '()))
       (cond
         ((eof-object? line) (reverse out))
         ((string-null? line) (reverse out))
         (else (loop (read-line port) (cons line out))))))

(define-public (array-for-each-index a proc-cell proc-endofline)
  (let* ((dims (array-dimensions a))
         (maxl (car dims))
         (maxc (cadr dims)))
    (do ((i 0 (1+ i)))
        ((>= i maxl))
      (do ((j 0 (1+ j)))
          ((>= j maxc))
        (proc-cell i j (array-ref a i j)))
      (proc-endofline i))))



(define-public (print-array a)
  (array-for-each-index
       a
       (lambda (i j v) (if v (display "X") (display ".")))
       (lambda (i) (display "\n"))))

(define (line->pair line)
   (let* (
          (x (string-split line #\-))
          (x (cons (car x) (cadr x))))
     x))


