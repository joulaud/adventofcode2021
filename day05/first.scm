(define-module (first)
    #:use-module (ice-9 rdelim)
    #:use-module (ice-9 match)
    #:use-module (ice-9 format)
    #:use-module (srfi srfi-1) ; lists library
    #:use-module (srfi srfi-11) ; let*-values
    #:use-module (srfi srfi-9)
    #:use-module (srfi srfi-9 gnu) ; immutable records
    #:use-module (srfi srfi-41))

;;(use-modules (ice-9 rdelim))
;;(use-modules (ice-9 match))
;;(use-modules (ice-9 format))
;;(use-modules (srfi srfi-1)) ; lists library
;;(use-modules (srfi srfi-11)) ; let*-values
;;(use-modules (srfi srfi-9))
;;(use-modules (srfi srfi-9 gnu)) ; immutable records
;;(use-modules (srfi srfi-41))

(define-record-type <vent-line>
  (make-vent-line x1 y1 x2 y2)
  vent-line?
  (x1 vent-line-x1)
  (y1 vent-line-y1)
  (x2 vent-line-x2)
  (y2 vent-line-y2))

(define (line->vent-line str)
  (let* ((separator (string-contains str " -> "))
         (coord1 (substring str 0 separator))
         (coord1 (string-split coord1 #\,))
         (coord1 (map string->number coord1))
         (coord2 (substring str (+ 4 separator)))
         (coord2 (string-split coord2 #\,))
         (coord2 (map string->number coord2)))
    (make-vent-line
         (car coord1) (cadr coord1)
         (car coord2) (cadr coord2))))

(define (vent-horiz-or-vert? vent-line)
  (or
   (eq? (vent-line-x1 vent-line) (vent-line-x2 vent-line))
   (eq? (vent-line-y1 vent-line) (vent-line-y2 vent-line))))

(define (direction vent-line)
  (cond
   ((eq? (vent-line-x1 vent-line) (vent-line-x2 vent-line)) 'horiz)
   ((eq? (vent-line-y1 vent-line) (vent-line-y2 vent-line)) 'vert)
   (#t 'oblique)))

(define (vent-line->path vent-line)
  (let (
        (x1 (vent-line-x1 vent-line))
        (y1 (vent-line-y1 vent-line))
        (x2 (vent-line-x2 vent-line))
        (y2 (vent-line-y2 vent-line)))
    (cond
      ((eq? x1 x2) ; horizontal line
       (let loop ((ycur (min y1 y2)) (ymax (max y1 y2)) (path '()))
            (if (= ycur ymax) (cons (cons x1 ycur) path)
                (loop (+ 1 ycur) ymax (cons (cons x1 ycur) path)))))
      ((eq? y1 y2) ; vertical line
       (let loop ((xcur (min x1 x2)) (xmax (max x1 x2)) (path '()))
            (if (= xcur xmax) (cons (cons xcur y1 ) path)
                (loop (+ 1 xcur) xmax (cons (cons xcur y1) path)))))
      (else '())))) ; oblique lines are just filtered-out

(define (grid-coverage vent-lines)
    ;; we suppose a 100x100 grid is reasonable
    (define h (make-hash-table (* 100 100)))
    (define (h-inc point)
        (let ((v (hash-ref h point)))
           (if v
               (hash-set! h point (+ v 1))
               (hash-set! h point 1))))
    (let ((paths (map vent-line->path vent-lines)))
      (for-each
        (lambda (path)
            (for-each
               (lambda (point)
                   (hash-inc point))
             path))
       paths))
    h)

(define-public (main args)
   (display "UNIMP"))
   ;(format #t (define-public (main args)
