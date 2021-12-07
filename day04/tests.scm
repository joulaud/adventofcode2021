(define-module (tests first)
    #:use-module (first)
    #:use-module (ice-9 rdelim)
    #:use-module (srfi srfi-19)
    #:use-module (srfi srfi-41)
    #:use-module (srfi srfi-64))


;; import internal functions of module to test
(define number-list->positions (@@ (first) number-list->positions))
(define make-bingo-grid (@@ (first) make-bingo-grid))
(define bingo-numbers (@@ (first) bingo-numbers))

(test-begin "parse bingo card")
(define h1 (number-list->positions '((1 2)(3 4))))
(test-equal '(1 . 1) (hash-ref h1 1))
(test-equal '(1 . 2) (hash-ref h1 2))
(test-equal '(2 . 1) (hash-ref h1 3))
(test-equal '(2 . 2) (hash-ref h1 4))
(define grid1 (make-bingo-grid '((5 6 7)(8 9 10))))
(test-equal '(2 . 3) (hash-ref (bingo-numbers grid1) 10))
(test-equal '(1 . 2) (hash-ref (bingo-numbers grid1) 6))
(test-end "parse bingo card")

