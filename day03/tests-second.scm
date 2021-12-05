(define-module (tests second)
    #:use-module (second)
    #:use-module (srfi srfi-19)
    #:use-module (srfi srfi-41)
    #:use-module (srfi srfi-64))

;; import internal functions of module to test
(define most-common (@@ (second) most-common))

(test-begin "extract most common number")
(test-eqv 0 (most-common '(0 0 0 0)))
(test-eqv 0 (most-common '(0 0 0 1)))
(test-eqv 1 (most-common '(0 0 1 1)))
(test-eqv 1 (most-common '(0 1 1 1)))
(test-eqv 1 (most-common '(1 1 1 1)))
(test-end "extract most common number")
