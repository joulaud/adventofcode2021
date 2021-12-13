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

(define-syntax-rule (import-private module)
    (define module (@@ (adventofcode2021 day13 origami) module)))

(define input-strm (stream-of-lines (open-input-file "inputs/example.txt")))
(stream-map display input-strm)

;; import internal functions of module to test
(import-private line->point)
(import-private make-point)

(test-begin "parsing input")
(test-equal (make-point 6 10) (line->point "6,10"))
(test-end "parsing input")


(import-private point-fold-vert)
(import-private point-fold-horiz)
(import-private first-fold)
(test-begin "folding")
(test-equal (make-point 0 0) (point-fold-vert 5 (make-point 10 0)))
(test-equal (make-point 0 0) (point-fold-horiz 5 (make-point 0 10)))
(test-equal 17 (first-fold (open-input-file "inputs/example.txt")))
(test-end "folding")
