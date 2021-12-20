(define-module (adventofcode2021 day19 beacons)
    #:use-module (adventofcode2021 day19 utils)
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
(use-modules (srfi srfi-2)) ; and-let*
(use-modules (srfi srfi-1)) ; lists library
(use-modules (srfi srfi-11)) ; let*-values
(use-modules (srfi srfi-9))
(use-modules (srfi srfi-9 gnu)) ; immutable records
(use-modules (srfi srfi-41))

(define (dbg t v) (format #t "~a~a\n" t v) (force-output))

(define-record-type <coord>
  (make-coord x y z)
  coord?
  (x coord-x)
  (y coord-y)
  (z coord-z))

(define (string->coord str)
  (let* ((x (string-split str #\,))
         (x (map string->number x)))
    (make-coord (first x) (second x) (third x))))

(define (read-scanner port)
  (let read-scanner-rec ((beacons '()))
    (let* ((line (read-line port)))
      (cond
       ((eof-object? line) (reverse beacons)) ; end-of-file
       ((string-null? line) (reverse beacons)) ; end-of-paragraph
       ((string-prefix? "--- scanner" line) (read-scanner-rec beacons)) ; begin of next paragraph
       (else
        (read-scanner-rec
          (cons (string->coord line) beacons)))))))

(define-public (main args)
   (let* (
          (result1 "UNIMP")
          (result2 "UNIMP"))
     (format #t "result1: ~a\n" result1)
     (format #t "result2: ~a\n" result2)))
