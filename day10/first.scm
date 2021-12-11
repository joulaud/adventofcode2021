(define-module (first)
    #:use-module (ice-9 rdelim)
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

(define (dbg t v) (format #t "~s: ~a\n" t v) (force-output))


(define pair-chars '(
                     (#\( . #\))
                     (#\[ . #\])
                     (#\{ . #\})
                     (#\< . #\>)))

(define (open-pair-char? c)
 (or
   (eqv? #\( c)
   (eqv? #\[ c)
   (eqv? #\{ c)
   (eqv? #\< c)
   (eqv? #\< c)))

(define (line-first-error line)
  (let loop ((line (string->list line))
             (opened '()))
       (cond
          ((eqv? '() line) #f)
          ((open-pair-char? (car line))
           (loop (cdr line) (cons (car line) opened)))
          ((eqv? (car line) (cdr (assoc (car opened) pair-chars)))
           (loop (cdr line) (cdr opened)))
          (else (car line)))))

(define (char->score c)
 (cdr
  (assoc c
   '(
      (#f . 0)
      (#\) . 3)
      (#\] . 57)
      (#\} . 1197)
      (#\> . 25137)))))

(char->score #f)

(define (err-score)
   (let loop ((total 0) (line (read-line)))
     (if (eof-object? line) total
         (let* ((err (line-first-error line))
                (cur-score (char->score err)))
            (loop (+ cur-score total) (read-line))))))


(define-public (main args)
  (let* (
         (result1 (err-score))
         (result2 "UNIMP"))
    (format #t "result1: ~a\n" result1)
    (format #t "result2: ~a\n" result2)))
