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


(define-public (main args)
   (display "UNIMP"))
   ;(format #t "result: ~d\n" (play-bingo))
