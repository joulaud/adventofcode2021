(define-module (tests second)
    #:use-module (second)
    #:use-module (srfi srfi-19)
    #:use-module (srfi srfi-41)
    #:use-module (srfi srfi-64))

;; import internal functions of module to test
(define most-common (@@ (second) most-common))
(define least-common (@@ (second) least-common))

(test-begin "extract most or least common number")
(test-eqv 0 (most-common '(0 0 0 0)))
(test-eqv 0 (most-common '(0 0 0 1)))
(test-eqv 1 (most-common '(0 0 1 1)))
(test-eqv 1 (most-common '(0 1 1 1)))
(test-eqv 1 (most-common '(1 1 1 1)))

(test-eqv 1 (least-common '(0 0 0 1)))
(test-eqv 0 (least-common '(0 1 1 1)))
;; default is 0 in case of tie
(test-eqv 0 (least-common '(0 0 1 1)))
;; 0 is not present at all, considered least common
(test-eqv 0 (least-common '(1 1 1 1)))
;; 1 is not present at all, considered least common
(test-eqv 1 (least-common '(0 0 0 0)))
(test-end "extract most or least common number")

;; import internal functions of module to test
(define o2-generator-rating-vec (@@ (second) o2-generator-rating-vec))
(define vec-of-binary-digits->num (@@ (second) vec-of-binary-digits->num))
(test-begin "ratings-from-diags")
;; testing termination condition on one list
(test-eqv #(0 0 0 0) (o2-generator-rating-vec (list #(0 0 0 0))))
;; testing only first byte
(test-eqv 
 #(1 1 1 1)
 (o2-generator-rating-vec
   (list #(0 0 0 1)
         #(1 1 1 1))))
;; testing first and second byte
(test-eqv
 #(1 1 1 1)
 (o2-generator-rating-vec
   (list #(0 0 0 1)
         #(0 0 1 1)
         #(1 0 1 1)
         #(1 1 1 1))))

;; testing first and second byte
(test-eqv
 #(1 1 1 1)
 (o2-generator-rating-vec
   (list #(0 0 0 1)
         #(0 0 1 1)
         #(1 0 1 1)
         #(1 1 1 1))))
(test-eqv
  23
  (vec-of-binary-digits->num #(1 0 1 1 1)))
(test-end "ratings-from-diags")
