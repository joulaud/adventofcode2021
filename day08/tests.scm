(define-module (tests first)
    #:use-module (first)
    #:use-module (ice-9 rdelim)
    #:use-module (srfi srfi-19)
    #:use-module (srfi srfi-41)
    #:use-module (srfi srfi-43)
    #:use-module (srfi srfi-64))

(use-modules (srfi srfi-1))
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

(define correctly-wired-result-bis
     '(
       ("bacfeg" . 0)
       ("fc" . 1)
       ("aedgc" . 2)
       ("dafgc" . 3)
       ("bdcf" . 4)
       ("afdbg" . 5)
       ("badfeg" . 6)
       ("caf" . 7)
       ("ebadcfg" . 8)
       ("badcfg" . 9)))




;; import internal functions of module to test
(define pattern-list->map  (@@ (first) pattern-list->map))

(test-begin "recognize-patterns")
(test-equal correctly-wired-result (pattern-list->map correctly-wired-patterns))
(test-equal correctly-wired-result-bis (pattern-list->map correctly-wired-patterns-bis))
(test-end "recognize-patterns")

;; import internal functions of module to test
(define parse-input-line  (@@ (first) parse-input-line))

(test-begin "parse-input")
(define input-line "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")
(test-equal
 '(("acedgfb" "cdfbe" "gcdfa" "fbcad" "dab" "cefabd" "cdfgeb" "eafb" "cagedb" "ab") "cdfeb" "fcadb" "cdfeb" "cdbaf")
 (parse-input-line input-line))
(test-end "parse-input")




;; import internal functions of module to test
(define count-1478  (@@ (first) count-1478))

(test-begin "counting-easy-numbers")
(define first-line-partial-mapping
  '(
    ("be" . 1)
    ("edb" . 7)
    ("cgeb" . 4)
    ("cfbegad" . 8)
    ("fdcge" . 10)
    ("fecdb" . 10)
    ("fabcd" . 10)
    ("cbdgef" . 10)
    ("fgaecd" . 10)
    ("agebfd" . 10)))
(define first-line-output '("fdgacbe" "cefdb" "cefbgd" "gcbe"))
(test-equal 2
            (count-1478 first-line-partial-mapping first-line-output))
(test-end "counting-easy-numbers")
