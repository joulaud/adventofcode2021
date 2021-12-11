(define-module (first)
    #:use-module (ice-9 rdelim)
    #:use-module (ice-9 match)
    #:use-module (ice-9 format)
    #:use-module (srfi srfi-1) ; lists library
    #:use-module (srfi srfi-2) ; and-let*
    #:use-module (srfi srfi-11) ; let*-values
    #:use-module (srfi srfi-9)
    #:use-module (srfi srfi-9 gnu) ; immutable records
    #:use-module (srfi srfi-41) ; Streams
    #:use-module (srfi srfi-43)) ; Vectors iterators

(use-modules (ice-9 rdelim))
(use-modules (ice-9 match))
(use-modules (ice-9 format))
(use-modules (srfi srfi-1)) ; lists library
(use-modules (srfi srfi-11)) ; let*-values
(use-modules (srfi srfi-9))
(use-modules (srfi srfi-9 gnu)) ; immutable records
(use-modules (srfi srfi-41))

(define (dbg t v) (format #t "~s: ~a\n" t v) (force-output))


(define-record-type <analysis>
  (make-analysis err opened)
  analysis?
  (err analysis-err)
  (opened analysis-opened))


(define stream-of-lines
  (stream-lambda (port)
     (let
         ((line (read-line port)))
      (cond
       ((eof-object? line) stream-null)
       (else (stream-cons line (stream-of-lines port)))))))

(define pair-chars '(
                     (#\( . #\))
                     (#\[ . #\])
                     (#\{ . #\})
                     (#\< . #\>)))

(define (open-pair-char? c)
 (or
   (eqv? #\( c)
   (eqv? #\[ c)
   (eqv? #\{ c)
   (eqv? #\< c)
   (eqv? #\< c)))

(define (line-status line)
  (let loop ((line (string->list line))
             (opened '()))
       (cond
          ((eqv? '() line) (make-analysis #f opened))
          ((open-pair-char? (car line))
           (loop (cdr line) (cons (car line) opened)))
          ((eqv? (car line) (cdr (assoc (car opened) pair-chars)))
           (loop (cdr line) (cdr opened)))
          (else (make-analysis (car line) opened)))))

(define (char->score c)
 (cdr
  (assoc c
   '(
      (#f . 0)
      (#\) . 3)
      (#\] . 57)
      (#\} . 1197)
      (#\> . 25137)))))

(char->score #f)

(define (char->autocomplete-score c)
 (cdr
  (assoc c
   '(
      (#f . 0)
      (#\( . 1)
      (#\[ . 2)
      (#\{ . 3)
      (#\< . 4)))))

(char->autocomplete-score #f)
(char->autocomplete-score (car (string->list "(")))
(char->autocomplete-score (car (string->list "[")))
(char->autocomplete-score (car (string->list "{")))
(char->autocomplete-score (car (string->list "<")))


(define-record-type <result>
  (mkres errscore autocompletescores)
  result?
  (errscore res-err)
  (autocompletescores res-auto))


(define (err-score)
   (let loop ((total 0) (autocompletes '()) (line (read-line)))
     (if (eof-object? line) (mkres total autocompletes)
         (let* ((analysis (line-status line))
                (cur-err (analysis-err analysis))
                (cur-errscore (char->score cur-err))
                (cur-autocomplete (analysis-opened analysis))
                (cur-autocompletescores (map char->autocomplete-score cur-autocomplete))
                (cur-autocompletescore (fold
                                        (lambda (cur acc) (+ cur (* 5 acc)))
                                        0 cur-autocompletescores))
                (cur-autocompletescore (if cur-err #f cur-autocompletescore)))
            (loop (+ cur-errscore total) (cons cur-autocompletescore autocompletes) (read-line))))))

;(with-input-from-string "[({(<(())[]>[[{[]{<()<>>" err-score)

(with-input-from-string "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]" err-score)

(define-public (main args)
  (let* (
         (result (err-score))
         (result1 (res-err result))
         (myresauto (filter identity (res-auto result)))
         (result2 (list-ref (sort myresauto <=) (quotient (length myresauto) 2))))
    (format #t "result1: ~a\n" result1)
    (format #t "result2: ~a\n" result2)))
