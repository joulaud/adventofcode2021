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

(define correctly-wired-result
     '(
       ("abcefg" . 0)
       ("cf" . 1)
       ("acdeg" . 2)
       ("acdfg" . 3)
       ("bcdf" . 4)
       ("abdfg" . 5)
       ("abdefg" . 6)
       ("acf" . 7)
       ("abcdefg" . 8)
       ("abcdfg" . 9)))

(define correctly-wired-patterns
  '("cf" "acf" "bcdf" "acdeg" "acdfg" "abdfg" "abcefg" "abdefg" "abcdfg" "abcdefg"))
(define correctly-wired-patterns-bis
  '("fc" "caf" "bdcf" "aedgc" "dafgc" "afdbg" "bacfeg" "badfeg" "badcfg" "ebadcfg"))

;; import internal functions of module to test
(define pattern-list->map  (@@ (first) pattern-list->map))

(test-begin "recognize-patterns")
(test-equal correctly-wired-result (pattern-list->map correctly-wired-patterns))
(test-end "recognize-patterns")
