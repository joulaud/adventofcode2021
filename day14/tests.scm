(define-module (adventofcode2021 day14 tests)
    #:use-module (adventofcode2021 day14 utils)
    #:use-module (adventofcode2021 day14 polymers)
    #:use-module (ice-9 rdelim)
    #:use-module (srfi srfi-1)
    #:use-module (srfi srfi-2) ; and-let*
    #:use-module (srfi srfi-19)
    #:use-module (srfi srfi-41)
    #:use-module (srfi srfi-43)
    #:use-module (srfi srfi-64))

(define (dbg t v) (format #t "~s: ~a\n" t v) (force-output))

(define-syntax-rule (import-private module)
    (define module (@@ (adventofcode2021 day14 polymers) module)))

(define (guess-current-dirname)
  (and-let* ((source-location (current-source-location))
             (filename (assq-ref source-location 'filename))
             (absolute-filepath (%search-load-path filename))
             (absolute-dirname (dirname absolute-filepath)))
         absolute-dirname))

(define testdir (or (guess-current-dirname) (getcwd)))

(define example-polymer-string "NNCB")
(define rule-bloc-string
    "CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C")

;; import internal functions of module to test
(import-private line->rule)
(import-private read-rules)
(import-private read-molecule)
(import-private molecule->molecule2)

(test-begin "parsing input")
(test-equal '((#\N . #\N) . #\C) (line->rule "NN -> C"))
(test-equal #\B (cdr (assoc '(#\C . #\H) (read-rules (open-input-string rule-bloc-string)))))
(test-equal '(#\N #\N #\C #\B) (read-molecule (open-input-string example-polymer-string)))
(test-equal '((#\N #\N) (#\N #\C) (#\C #\B)) (molecule->molecule2 (read-molecule (open-input-string example-polymer-string))))
(test-end "parsing input")



(test-begin "some steps")
(test-end "some steps")

