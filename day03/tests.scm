(define-module (tests first)
    #:use-module (first)
    #:use-module (srfi srfi-19)
    #:use-module (srfi srfi-41)
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

;; import internal functions of module to test
(define make-binary-counter (@@ (first) make-binary-counter))
(define count-binary-in-diagnostic-stream (@@ (first) count-binary-in-diagnostic-stream))
(define list-of-5-binary-counter-empty (@@ (first) list-of-5-binary-counter-empty))
(define add-to-list-of-binary-counter (@@ (first) add-to-list-of-binary-counter))

(test-begin "count bits in stream of diagnostic lines")
(define input-list '(#\0 #\0 #\1 #\1 #\0))
(define expected (list
                  (make-binary-counter 1 0)
                  (make-binary-counter 1 0)
                  (make-binary-counter 0 1)
                  (make-binary-counter 0 1)
                  (make-binary-counter 1 0)))
(define res1 (add-to-list-of-binary-counter list-of-5-binary-counter-empty input-list))
(test-equal res1 expected)
(define input-strm (stream input-list))
(define res2 (count-binary-in-diagnostic-stream input-strm 5))
(test-equal res2 expected)
(test-end "count bits in stream of diagnostic lines")

;; import internal functions of module to test
(define min-max-from-counter (@@ (first) min-max-from-counter))
(define gamma (@@ (first) gamma))
(define epsilon (@@ (first) epsilon))
(test-begin "extract gamma and epsilon from counter")
; 1 zero, 0 ones, min is one, max is zero
(test-equal (cons 1 0) (min-max-from-counter (make-binary-counter 1 0)))
; same
(test-equal (cons 1 0) (min-max-from-counter (make-binary-counter 5 2)))
; here we have more 1s than 0s
(test-equal (cons 0 1) (min-max-from-counter (make-binary-counter 4 6)))
(define input (list
               (make-binary-counter 5 7)
               (make-binary-counter 7 5)
               (make-binary-counter 4 8)
               (make-binary-counter 5 7)
               (make-binary-counter 7 5)))
(test-eqv 22 (gamma input))
(test-eqv 9 (epsilon input))
(test-end "extract gamma and epsilon from counter")


(define get-power-consumption (@@ (first) get-power-consumption))
(test-begin "extract power consumption from diagnostic")
(test-eqv 198
  (with-input-from-string "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"
      get-power-consumption))
(test-end "extract power consumption from diagnostic")
