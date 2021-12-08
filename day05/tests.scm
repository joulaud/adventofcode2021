(define-module (tests first)
    #:use-module (first)
    #:use-module (ice-9 rdelim)
    #:use-module (srfi srfi-19)
    #:use-module (srfi srfi-41)
    #:use-module (srfi srfi-64))



(define input-string
         "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")

;; import internal functions of module to test
(define make-vent-line (@@ (first) make-vent-line))
(define line->vent-line (@@ (first) line->vent-line))

(test-begin "parse vent")
(test-equal (make-vent-line '(8 . 0) '(0 . 8)) (line->vent-line "8,0 -> 0,8"))
(test-begin "parse vent")
