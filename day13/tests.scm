(define-module (adventofcode2021 day13 tests)
    #:use-module (adventofcode2021 day13 utils)
    #:use-module (adventofcode2021 day13 origami)
    #:use-module (ice-9 rdelim)
    #:use-module (srfi srfi-1)
    #:use-module (srfi srfi-19)
    #:use-module (srfi srfi-41)
    #:use-module (srfi srfi-43)
    #:use-module (srfi srfi-64))

(define (dbg t v) (format #t "~s: ~a\n" t v) (force-output))

(define input-strm (stream-of-lines (open-input-file "inputs/example.txt")))
(stream-map display input-strm)


;; import internal functions of module to test
;;(define graph-neighbours (@@ (first) graph-neighbours))

(test-begin "parsing input")
(test-end "parsing input")

