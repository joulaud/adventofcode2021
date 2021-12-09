(define-module (tests first)
    #:use-module (first)
    #:use-module (ice-9 rdelim)
    #:use-module (srfi srfi-19)
    #:use-module (srfi srfi-41)
    #:use-module (srfi srfi-43)
    #:use-module (srfi srfi-64))

(use-modules (srfi srfi-1))
(define (dbg t v) (format #t "~s: ~a\n" t v) (force-output))

(define input-string "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")

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
(define count-all-1478  (@@ (first) count-all-1478))
(define parse-all  (@@ (first) parse-all))

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
            (count-1478 first-line-partial-mapping first-line-output
             (mapping (pattern-list->map patterns))))
(test-equal 26 (count-all-1478 (parse-all (open-input-string input-string) 0)))
(test-end "counting-easy-numbers")
(parse-all (open-input-string input-string))
(read-line $20)
