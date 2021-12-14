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
(import-private molecule2-pairs)
(import-private make-pair)
(import-private pair-a)
(import-private pair-b)
(import-private molecule2->result)

(test-begin "parsing input")
(test-equal '((#\N . #\N) . #\C) (line->rule "NN -> C"))
(test-equal #\B (cdr (assoc '(#\C . #\H) (read-rules (open-input-string rule-bloc-string)))))
(test-equal '(#\N #\N #\C #\B) (read-molecule (open-input-string example-polymer-string)))

(define (mysort l)
  (sort
    l
    (lambda (x y)
        (cond
          ((= (cdr x) (cdr y))
           (if (char=?  (pair-a (car x)) (pair-a (car y)))
               (char<=? (pair-b (car x)) (pair-b (car y)))
               (char<=? (pair-a (car x)) (pair-a (car y)))))
          (else (<= (cdr x) (cdr y)))))))
(test-equal
 (mysort `((,(make-pair #\N #\N) . 1) (,(make-pair #\N #\C) . 1) (,(make-pair #\C #\B) . 1)))
 (mysort (molecule2-pairs (molecule->molecule2 (read-molecule (open-input-string example-polymer-string))))))
(test-end "parsing input")

(test-begin "representation equivalence")
(define (molecule) (molecule->molecule2 (read-molecule (open-input-string example-polymer-string))))
(define (mypairs) (molecule2-pairs (molecule)))
(define rules (rules->rules2 (read-rules (open-input-string rule-bloc-string))))
(define step1 (one-step2 (molecule) rules))
(test-equal 1 (molecule2->result step1))
(test-end "representation equivalence")

(test-begin "some steps")

(test-end "some steps")

(define (print-pairs text pairs)
  (format #t text)
  (for-each
    (lambda (x)
        (format #t "~a,~a: ~a\n" (pair-a (car x)) (pair-b (car x)) (cdr x)))
   (mysort pairs)))

