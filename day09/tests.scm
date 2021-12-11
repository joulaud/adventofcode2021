(define-module (tests first)
    #:use-module (first)
    #:use-module (ice-9 rdelim)
    #:use-module (srfi srfi-19)
    #:use-module (srfi srfi-41)
    #:use-module (srfi srfi-43)
    #:use-module (srfi srfi-64))

(use-modules (srfi srfi-1))
(define (dbg t v) (format #t "~s: ~a\n" t v) (force-output))

(define input-string "2199943210
3987894921
9856789892
8767896789
9899965678")

(define example-array
    #2((2 1 9 9 9 4 3 2 1 0)
       (3 9 8 7 8 9 4 9 2 1)
       (9 8 5 6 7 8 9 8 9 2)
       (8 7 6 7 8 9 6 7 8 9)
       (9 8 9 9 9 6 5 6 7 8)))

(define stream-of-lines  (@@ (first) stream-of-lines))
(define input-strm (stream-of-lines (open-input-string input-string)))

;; import internal functions of module to test
(define parse-line (@@ (first) parse-line))
(define stream-of-lines->array (@@ (first) stream-of-lines->array))

(test-begin "parsing input")
(test-equal '(1 2 3 4 5) (parse-line "12345"))
(test-equal
 example-array
 (stream-of-lines->array input-strm))
(test-end "parsing input")

(test-begin "neighbours list")
(define neighbours  (@@ (first) neighbours))
(test-equal 8 (length (neighbours 1 1 3 3)))
(test-equal 3 (length (neighbours 0 0 3 3)))
(test-equal 5 (length (neighbours 1 2 3 3)))
(test-end "neighbours list")

;(define x (@@ (first) x))
(define low? (@@ (first) low?))

(test-begin "low points")
(test-equal #t (low? example-array 0 1))
(test-equal #t (low? example-array 0 1))
(test-equal #f (low? example-array 0 0))
(test-equal #f (low? example-array 2 4))
(test-end "low points")

;; import internal functions of module to test
(define risk-level  (@@ (first) risk-level))

(test-begin "risk-level")
(test-equal 15 (risk-level example-array))
(test-end "risk-level")

;; import internal functions of module to test
(define basin-size  (@@ (first) basin-size))
(define basin-sizes  (@@ (first) basin-sizes))
(define result2  (@@ (first) result2))

(test-begin "fill-basin")
(test-equal 14 (basin-size example-array (cons 2 2)))
(test-equal 9 (basin-size example-array (cons 0 9)))
(test-equal 3 (basin-size example-array (cons 0 1)))
(test-equal 9 (basin-size example-array (cons 4 6)))
(test-equal '(9 14 9 3) (basin-sizes example-array))
(test-equal 1134 (result2 example-array))
(test-end "fill-basin")
