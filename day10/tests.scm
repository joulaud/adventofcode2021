(define-module (tests first)
    #:use-module (first)
    #:use-module (ice-9 rdelim)
    #:use-module (srfi srfi-19)
    #:use-module (srfi srfi-41)
    #:use-module (srfi srfi-43)
    #:use-module (srfi srfi-64))

(use-modules (srfi srfi-1))
(use-modules (ice-9 rdelim))
(define (dbg t v) (format #t "~s: ~a\n" t v) (force-output))


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

(define (line-first-error line)
  (let loop ((line (string->list line))
             (opened '()))
       (cond
          ((eqv? '() line) #f)
          ((open-pair-char? (car line))
           (loop (cdr line) (cons (car line) opened)))
          ((eqv? (car line) (cdr (assoc (car opened) pair-chars)))
           (loop (cdr line) (cdr opened)))
          (else (car line)))))

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

(define (err-score)
   (let loop ((total 0) (line (read-line)))
     (if (eof-object? line) total
         (let* ((err (line-first-error line))
                (cur-score (char->score err)))
            (loop (+ cur-score total) (read-line))))))


(line-first-error "{([(<{}[<>[]}>{[]{[(<()>") ;; - Expected ], but found } instead.
(line-first-error "[[<[([]))<([[{}[[()]]]") ;; - Expected ], but found ) instead.
(line-first-error "[{[{({}]{}}([{[{{{}}([]") ;; - Expected ), but found ] instead.
(line-first-error "[<(<(<(<{}))><([]([]()") ;; - Expected >, but found ) instead.
(line-first-error "<{([([[(<>()){}]>(<<{{") ;; - Expected ], but found > instead.

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

