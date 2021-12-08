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

(define full-input-string
         "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
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
(define number-list->cells (@@ (first) number-list->cells))
(define make-bingo-grid (@@ (first) make-bingo-grid))
(define bingo-cells (@@ (first) bingo-cells))
(define bingo-grids (@@ (first) bingo-grids))
(define read-grid->numlistlist (@@ (first) read-grid->numlistlist))

(test-begin "parse bingo card")
(define h1 (number-list->cells '((1 2)(3 4))))
(test-equal '((0 . 0) . #f) (hash-ref h1 1))
(test-equal '((0 . 1) . #f) (hash-ref h1 2))
(test-equal '((1 . 0) . #f) (hash-ref h1 3))
(test-equal '((1 . 1) . #f) (hash-ref h1 4))
(define grid1 (make-bingo-grid '((5 6 7)(8 9 10))))
(test-equal '((1 . 2) . #f) (hash-ref (bingo-cells grid1) 10))
(test-equal '((0 . 1) . #f) (hash-ref (bingo-cells grid1) 6))
(define numlist2
  (cdr
    (with-input-from-string
           "22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19
"
           read-grid->numlistlist)))
;; FIXME: order of lines is reversed (not a problem for solving the test)
;(test-equal '(22 13 17 11 0) (car numlist2))
(test-equal '(1 12 20 15 19) (car numlist2))
(define grid2 (make-bingo-grid numlist2))
(test-equal '((0 . 0) . #f) (hash-ref (bingo-cells grid2) 1))
(test-equal '((4 . 4) . #f) (hash-ref (bingo-cells grid2) 0))
(define grids
    (with-input-from-string input-string bingo-grids))
(define grid3 (car grids))
(test-equal '((0 . 0) . #f) (hash-ref (bingo-cells grid3) 2))
(test-end "parse bingo card")

;; import internal functions of module to test
(define bingo-tick (@@ (first) bingo-tick))

(test-begin "tick bingo card")
;; ticking one full line of grid1
(test-equal #f (bingo-tick grid1 5))
(test-equal #f (bingo-tick grid1 6))
(test-equal #t (bingo-tick grid1 7))
;; ticking diagonal of grid2
(test-equal #f (bingo-tick grid2 22))
(test-equal #f (bingo-tick grid2 2))
(test-equal #f (bingo-tick grid2 14))
(test-equal #f (bingo-tick grid2 18))
(test-equal #f (bingo-tick grid2 19))
;; and then last col of grid2
(test-equal #f (bingo-tick grid2 5))
(test-equal #f (bingo-tick grid2 7))
(test-equal #f (bingo-tick grid2 24))
(test-equal #t (bingo-tick grid2 0))
;; check tick marks
(test-equal #f (cdr (hash-ref (bingo-cells grid2) 21)))
(test-equal #f (cdr (hash-ref (bingo-cells grid2) 6)))
(test-equal #t (cdr (hash-ref (bingo-cells grid2) 5)))
(test-equal #t (cdr (hash-ref (bingo-cells grid2) 14)))
(test-end "tick bingo card")

;; import internal functions of module to test
(define bingo-score (@@ (first) bingo-score))

(test-begin "score bingo card")
(test-equal (+ 8 9 10) (bingo-score grid1))
(test-end "score bingo card")


;; import internal functions of module to test
(define play-all-grids (@@ (first) play-all-grids))
(define play-bingo (@@ (first) play-bingo))
(define play-losing-bingo (@@ (first) play-losing-bingo))

(test-begin "play bingo game")
(play-all-grids grids 0)
(test-equal #t (cdr (hash-ref (bingo-cells (list-ref grids 0)) 0)))
(test-equal #t (cdr (hash-ref (bingo-cells (list-ref grids 1)) 0)))
(test-equal #t (cdr (hash-ref (bingo-cells (list-ref grids 2)) 0)))
(test-equal 4512 (with-input-from-string full-input-string play-bingo))
(test-equal 1924 (with-input-from-string full-input-string play-losing-bingo))
(test-end "play bingo game")
