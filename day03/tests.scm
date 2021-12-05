(define-module (tests first)
    #:use-module (first)
    #:use-module (srfi srfi-19)
    #:use-module (srfi srfi-64))

;; import internal functions of module to test
(define binary-counter-empty (@@ (first) binary-counter-empty))
(define count0 (@@ (first) count0))
(define count1 (@@ (first) count1))
(define inc0 (@@ (first) inc0))
(define inc1 (@@ (first) inc1))

(test-begin "binary-counter functions")
(test-eqv (count0 binary-counter-empty) 0)
(test-eqv (count1 binary-counter-empty) 0)
(define one1 (inc1 binary-counter-empty))
(define one0 (inc0 binary-counter-empty))
(test-eqv (count0 one1) 0)
(test-eqv (count1 one1) 1)
(test-eqv (count0 one0) 1)
(test-eqv (count1 one0) 0)
(test-end "binary-counter functions")

;; import internal functions of module to test
(define add-char-to-counter (@@ (first) add-char-to-counter))

(test-begin "char to binary-counter functions")
(define a (add-char-to-counter binary-counter-empty #\0))
(define b (add-char-to-counter a #\0))
(define c (add-char-to-counter b #\1))
(test-eqv (count0 a) 1)
(test-eqv (count1 a) 0)
(test-eqv (count0 b) 2)
(test-eqv (count1 b) 0)
(test-eqv (count0 c) 2)
(test-eqv (count1 c) 1)
(test-end"char to binary-counter functions")

(define (x y)
    (cond
          ((eqv? y 1) 1)))
(x 2)
