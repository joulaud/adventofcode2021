(define-module (adventofcode2021 day20 trench)
    #:use-module (adventofcode2021 day20 utils)
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
    #:use-module (srfi srfi-26)) ; cut (specializing parameters, currying alternative)

(use-modules (ice-9 rdelim))
(use-modules (ice-9 match))
(use-modules (ice-9 format))
(use-modules (srfi srfi-2)) ; and-let*
(use-modules (srfi srfi-1)) ; lists library
(use-modules (srfi srfi-11)) ; let*-values
(use-modules (srfi srfi-9))
(use-modules (srfi srfi-9 gnu)) ; immutable records
(use-modules (srfi srfi-41))
(use-modules (srfi srfi-26)) ; cut (specializing parameters, currying alternative)

(define (dbg t v) (format #t "~a~a\n" t v) (force-output))
(define (dbgn t v) (format #t "~a~a  #  " t v) (force-output))

(define-record-type <point>
  (make-point x y)
  point?
  (x point-x)
  (y point-y))

(define (point<? a b)
  (or
   (< (point-x a) (point-x b))
   (and (= (point-x a) (point-x b))
        (< (point-y a) (point-y b)))))

(define-record-type <image>
  (make-image point-min point-max points)
  image?
  (point-min image-point-min)
  (point-max image-point-max)
  (points image-points))

(define (parse-enhancement line)
   (map
    (cut char=? #\# <>)
    (string->list line)))

(define (parse-image port)
  (let parse-image-rec ((line 0) (col 0) (points '()) (maxpoint #f))
     (let ((c (read-char port)))
       (dbg "c=" c)
       (cond
         ((eof-object? c) (make-image (make-point 0 0) maxpoint (reverse points)))
         ((char=? #\newline c) (parse-image-rec (1+ line) 0
                                                points                                   maxpoint))
         ((char=? #\# c) (parse-image-rec line     (1+ col)
                                          (acons (make-point line col) #t points) (make-point line col)))
         ((char=? #\. c) (parse-image-rec line     (1+ col)
                                          points (make-point line col)))
         (else (error (string-append "parsing failed: " c)))))))

(define (input-value point points)
 (let* ((x (point-x point))
        (y (point-y point))
        (n  (list
             (assoc (make-point (1- x) (1- y)) points)
             (assoc (make-point     x  (1- y)) points)
             (assoc (make-point (1+ x) (1- y)) points)
             (assoc (make-point (1- x)     y ) points)
             (assoc (make-point     x      y ) points)
             (assoc (make-point (1+ x)     y ) points)
             (assoc (make-point (1- x) (1+ y)) points)
             (assoc (make-point     x  (1+ y)) points)
             (assoc (make-point (1+ x) (1+ y)) points)))
        (n (fold (lambda (cur num) (+ (* 2 num) (if cur 1 0)))
                 0
                 n)))
    n))

(define (output-pixel value enhancement)
    (list-ref enhancement value))

(define (image-line->string image x)
  (let* ((p-max (image-point-max image))
         (p-min (image-point-min image))
         (y-min (1- (point-y p-min)))
         (y-max (1+ (point-y p-max)))
         (points (image-points image)))
    (let loop ((y y-min) (line '()))
       (cond
        ((> y y-max) (list->string (reverse (cons #\newline line))))
        (else
         (let ((char (if (assoc (make-point x y) points)
                      #\#
                      #\.)))
           (loop (1+ y) (cons char line))))))))

(define (image->string image)
  (let* ((p-max (image-point-max image))
         (p-min (image-point-min image))
         (x-min (1- (point-x p-min)))
         (x-max (1+ (point-x p-max))))
    (let loop ((x x-min) (lines '()))
       (cond
        ((> x x-max) (string-concatenate (reverse lines)))
        (else
           (loop (1+ x) (cons (image-line->string image x) lines)))))))

(define-public (main args)
   (let* (
          (result1 "UNIMP")
          (result2 "UNIMP"))
     (format #t "result1: ~a\n" result1)
     (format #t "result2: ~a\n" result2)))
