(define-module (adventofcode2021 day15 escapepath)
    #:use-module (adventofcode2021 day15 utils)
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

(define (read-cavemap port)
  (let* ((x (stream-of-lines port))
         (x (stream-of-lines->array line->numlist x)))
    x))

(define-record-type <coord>
    (make-coord line col)
    coord?
    (line coord-line)
    (col coord-col))
(define (coord->pair coord)
   (cons (coord-line coord) (coord-col coord)))
(define (pair->coord pair)
   (make-coord (car pair) (cdr pair)))
(define (array->gridsize grid)
  (let ((dims (array-dimensions grid)))
   (make-coord (car dims) (cadr dims))))

(define (ingrid? gridsize)
 (lambda (loc)
  (let* (
         (l (coord-line loc))
         (c (coord-col loc))
         (lmax (coord-line gridsize))
         (cmax (coord-col gridsize)))
    (and (>= l 0)
         (>= c 0)
         (<= l lmax)
         (<= c cmax)))))

(define (neighbours loc gridsize)
  (let* ((up    '(-1 . 0))
         (down  '(1 . 0))
         (left  '(0 . -1))
         (right '(0 . 1))
         (l (coord-line loc))
         (c (coord-col loc))
         (neighbours (map
                       (lambda (direction)
                           (make-coord (+ l (car direction))
                                       (+ c (cdr direction))))
                       (list up down left right)))
         (neighbours (filter
                      (ingrid? gridsize)
                      neighbours)))
     neighbours))

(define cavemap-ref array-ref)

(define-public (main args)
  (let* ((result1 "UNIMP")
         (result2 "UNIMP"))
     (format #t "result1: ~a\n" result1)
     (format #t "result2: ~a\n" result2)))
