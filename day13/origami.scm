(define-module (adventofcode2021 day13 origami)
    #:use-module (adventofcode2021 day13 utils)
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

(define-record-type <point>
  (make-point x y)
  point?
  (x point-x)
  (y point-y))
(define (line->point str)
  (let ((pair (string-split str #\,)))
    (make-point
      (string->number (car pair))
      (string->number (cadr pair)))))

(define (point-fold-vert along-x point)
   (let ((x (point-x point)))
      (cond
       ((= x along-x) #f) ; discard points on the folding line
       ((> x along-x) (make-point (- (* 2 along-x) x) (point-y point)))
       (else point))))

(define (point-fold-horiz along-y point)
   (let ((y (point-y point)))
      (cond
       ((= y along-y) #f)
       ((> y along-y) (make-point (point-x point) (- (* 2 along-y) y)))
       (else point))))

(define-record-type <sheet>
   (list->sheet points)
   sheet?
   (points sheet->list))

(define (lines->sheet lines)
   (list->sheet
     (map
      line->point
      lines)))

(define (sheet-print sheet)
  (let* ((lst (sheet->list sheet))
         (max-point (fold
                     (lambda (point maxes)
                        (make-point
                          (max (point-x point) (point-x maxes))
                          (max (point-y point) (point-y maxes))))
                     (make-point 0 0)
                     lst))
         (xmax (point-x max-point))
         (ymax (point-y max-point)))
     (do ((y 0 (1+ y)))
         ((> y ymax))
       (do ((x 0 (1+ x)))
           ((> x xmax))
         (if (member (make-point x y) lst)
             (display "#")
             (display ".")))
       (display "\n"))))





;; (define-record-type <fold-inst>
;;   (make-fold-inst type val)
;;   fold-inst?
;;   (type fold-type)
;;   (val fold-val))
;; 
;; (define (line->fold-instruction line)
;;   (let* ((isfold? (string-prefix? "fold along" line))
;;          (type (string-ref line 11))
;;          (val (cadr (string-split line #\=)))
;;          (val (string->number val)))
;;     (make-fold-inst type val)))


(define (line->fold-instruction line)
  (let* ((isfold? (string-prefix? "fold along" line))
         (type (string-ref line 11))
         (val (cadr (string-split line #\=)))
         (val (string->number val)))
    (cond
      ((eqv? #\x type)
       (lambda (p) (point-fold-vert val p)))
      ((eqv? #\y type)
       (lambda (p) (point-fold-horiz val p))))))

(define (sheet-fold sheet fold-inst)
   (let* ((points (sheet->list sheet))
          (new-points (map
                        fold-inst
                        points))
          (new-points (filter identity new-points)))
     (list->sheet new-points)))

(define (first-fold port)
   (let* ((sheet (read-bloc port))
          (sheet (lines->sheet sheet))
          (first-inst (read-line port))
          (first-inst (line->fold-instruction first-inst))
          (folded (sheet-fold sheet first-inst)))
     (length (sheet->list folded))))

(define-public (main args)
  (let* (
         (result1 "UNImP")
         (result2 "UNIMP"))
    (format #t "result1: ~a\n" result1)
    (format #t "result2: ~a\n" result2)))
