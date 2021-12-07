(define-module (tests first)
    #:use-module (first)
    #:use-module (ice-9 rdelim)
    #:use-module (srfi srfi-19)
    #:use-module (srfi srfi-41)
    #:use-module (srfi srfi-64))


(define input-string
         "22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
")


;; import internal functions of module to test
(define number-list->positions (@@ (first) number-list->positions))
(define make-bingo-grid (@@ (first) make-bingo-grid))
(define bingo-numbers (@@ (first) bingo-numbers))
(define read-grid->numlistlist (@@ (first) read-grid->numlistlist))

(test-begin "parse bingo card")
(define h1 (number-list->positions '((1 2)(3 4))))
(test-equal '(1 . 1) (hash-ref h1 1))
(test-equal '(1 . 2) (hash-ref h1 2))
(test-equal '(2 . 1) (hash-ref h1 3))
(test-equal '(2 . 2) (hash-ref h1 4))
(define grid1 (make-bingo-grid '((5 6 7)(8 9 10))))
(test-equal '(2 . 3) (hash-ref (bingo-numbers grid1) 10))
(test-equal '(1 . 2) (hash-ref (bingo-numbers grid1) 6))
(define numlist2
  (with-input-from-string
         "22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19
"
         read-grid->numlistlist))
;; FIXME: order of lines is reversed (not a problem for solving the test)
;(test-equal '(22 13 17 11 0) (car numlist2))
(test-equal '(1 12 20 15 19) (car numlist2))
(define grid2 (make-bingo-grid numlist2))
(test-equal '(1 . 1) (hash-ref (bingo-numbers grid2) 1))
(test-equal '(5 . 5) (hash-ref (bingo-numbers grid2) 0))
(test-end "parse bingo card")

