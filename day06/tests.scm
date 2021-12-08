(define-module (tests first)
    #:use-module (first)
    #:use-module (ice-9 rdelim)
    #:use-module (srfi srfi-19)
    #:use-module (srfi srfi-41)
    #:use-module (srfi srfi-43)
    #:use-module (srfi srfi-64))

(define (dbg t v) (format #t "~s: ~a\n" t v) (force-output))

(define input-string "3,4,3,1,2")
(define day1-string "2,3,2,0,1")
(define day10-string "0,1,0,5,6,0,1,2,2,3,7,8")
(define input-port)

;; import internal functions of module to test
(define port->school (@@ (first) port->school))
(define one-day  (@@ (first) one-day))
(define iterate (@@ (first) iterate))
(define count-lanternfishes (@@ (first) count-lanternfishes))

(test-begin "parse school")
(dbg "day0" (port->school (open-input-string input-string)))
(test-assert (vector= eq? #(0 1 1 2 1 0 0 0 0) (port->school (open-input-string input-string))))
(define day0
  (port->school (open-input-string input-string)))
(test-assert (vector= eq? (port->school (open-input-string day1-string)) (one-day day0)))
(test-assert (vector= eq? (port->school (open-input-string day1-string)) (iterate day0 1)))
(test-assert (vector= eq? (port->school (open-input-string day10-string)) (iterate day0 10)))
(test-equal 5934 (count-lanternfishes (iterate day0 80)))
(test-end "parse school")
