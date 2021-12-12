(define-module (tests first)
    #:use-module (first)
    #:use-module (ice-9 rdelim)
    #:use-module (srfi srfi-19)
    #:use-module (srfi srfi-41)
    #:use-module (srfi srfi-43)
    #:use-module (srfi srfi-64))

(use-modules (srfi srfi-1))
(define (dbg t v) (format #t "~s: ~a\n" t v) (force-output))

(define input-string "start-A
start-b
A-c
A-b
b-d
A-end
b-end
other-end
island1-island2")

(define stream-of-lines  (@@ (first) stream-of-lines))
(define input-strm (stream-of-lines (open-input-string input-string)))

;; import internal functions of module to test
(define stream-of-lines->graph (@@ (first) stream-of-lines->graph))
(define node-neighbours (@@ (first) node-neighbours))
(define graph-neighbours (@@ (first) graph-neighbours))

(test-begin "parsing input")
(define input-graph (stream-of-lines->graph input-strm))
(test-equal
 (sort '("c" "b" "end" "start") string<=?)
 (sort (graph-neighbours input-graph "A") string<=?))
(test-end "parsing input")


;; import internal functions of module to test
(define graph->paths (@@ (first) graph->paths))
(define make-path (@@ (first) make-path))

(test-begin "paths extraction")
(test-equal '() (graph->paths input-graph "island1"))
(test-equal (list (make-path '("other" "end"))) (graph->paths input-graph "other"))
(test-equal 10 (length (graph->paths input-graph "start")))
(test-end "paths extraction")

