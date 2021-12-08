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
  (make-vent-line orig dest)
  vent-line?
  (orig vent-line-orig)
  (dest vent-line-dest))

(define (line->vent-line str)
  (let* ((separator (string-contains str " -> "))
         (coord-orig (substring str 0 separator))
         (coord-orig (string-split coord-orig #\,))
         (coord-orig (map string->number coord-orig))
         (coord-orig (cons (car coord-orig) (cadr coord-orig)))
         (coord-dest (substring str (+ 4 separator)))
         (coord-dest (string-split coord-dest #\,))
         (coord-dest (map string->number coord-dest))
         (coord-dest (cons (car coord-dest) (cadr coord-dest))))
    (make-vent-line coord-orig coord-dest)))

(define-public (main args)
   ;(format #t "result: ~d\n" (play-bingo))
   (format #t "result2: ~d\n" (play-losing-bingo)))
