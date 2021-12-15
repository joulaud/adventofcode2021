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

(define-record-type <coord>
    (make-coord line col)
    coord?
    (line coord-line)
    (col coord-col))
(define (coord->pair coord)
   (cons (coord-line coord) (coord-col coord)))
(define (pair->coord pair)
   (make-coord (car pair) (cdr pair)))
(define (array->maxcoord grid)
  (let ((dims (array-dimensions grid)))
   (make-coord (1- (car dims)) (1- (cadr dims)))))


(define (read-cavemap port)
  (let* ((x (stream-of-lines port))
         (x (stream-of-lines->array line->numlist x)))
    x))

(define cavemap-ref array-ref)
(define (cavemap-refloc cavemap loc)
     (array-ref cavemap (coord-line loc) (coord-col loc)))



(define (ingrid? maxcoord)
 (lambda (loc)
  (let* (
         (l (coord-line loc))
         (c (coord-col loc))
         (lmax (coord-line maxcoord))
         (cmax (coord-col maxcoord)))
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

(define (ormin a b)
    (cond
     ((and a b) (min a b))
     (a a)
     (b b)
     (else #f)))

(define (lowest-risk cavemap visited loc initialrisk)
   (let* ((endloc (array->maxcoord cavemap))
          (currisk (cavemap-refloc cavemap loc))
          (updatedrisk (+ currisk initialrisk))
          (curvisited (assoc loc visited))
          (curvisited? (and curvisited (cdr curvisited)))
          (end? (equal? loc endloc))
          (updatedvisited (acons loc #t visited))
          (neighbours (neighbours loc endloc)))
     (cond
      (curvisited? #f)
      (end? updatedrisk)
      (else
       (let* ((subrisks
               (map
                (lambda (x) (lowest-risk cavemap updatedvisited x updatedrisk))
                neighbours))
              (minrisk (fold ormin #f subrisks)))
           minrisk)))))







(define-public (main args)
  (let* ((result1 "UNIMP")
         (result2 "UNIMP"))
     (format #t "result1: ~a\n" result1)
     (format #t "result2: ~a\n" result2)))
