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

;; import internal functions of module to test
(define o2-generator-rating (@@ (second) o2-generator-rating))
(test-begin "o2-generator-rating")
;; testing termination condition on one list
(test-eqv '(0 0 0 0) (o2-generator-rating '((0 0 0 0))))
;; testing only first pass
(test-eqv 
 '(1 1 1 1)
 (o2-generator-rating
  '((0 0 0 1)
    (0 0 1 1)
    (0 1 1 1)
    (1 1 1 1))))
(test-end "o2-generator-rating")

