(define-module (adventofcode2021 day14 polymers)
    #:use-module (adventofcode2021 day14 utils)
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

(define-record-type <molecule2>
   (make-molecule2 pairs first last)
   molecule2?
   (pairs molecule2-pairs)
   (first molecule2-first)
   (last molecule2-last))
(define-record-type <pair>
  (make-pair a b)
  pair?
  (a pair-a)
  (b pair-b))

(define (molecule->molecule2 molecule)
 (let* ((pairs (count-elem
                 (zip molecule (cdr molecule))))
        (nul (dbg "pairs" pairs))
        (nul (dbg "car pairs" (car pairs)))
        (pairs (map
                 (lambda (x) (cons (cons (caar x) (cadar x)) (cdr x)))
                 pairs))
        (nul (dbg "pairs" pairs))
        (last (list-ref molecule (1- (length molecule)))))
   (make-molecule2 pairs (car molecule) last)))

(define (one-step molecule rules)
  (let loop ((molecule molecule) (result '()))
      (if (<= (length molecule) 1)
          (reverse (cons (car molecule) result))
          (let* ((a (car molecule))
                 (b (cadr molecule))
                 (n (assoc (cons a b) rules))
                 (n (if n (cdr n) n))
                 (result (cons a result))
                 (result (if n (cons n result) result)))
            (loop (cdr molecule) result)))))

(define (decrease! k alist)
   (let* ((oldval (assoc k alist))
          (oldval (if oldval (cdr oldval) #f)))
     (if oldval
         (assoc-set! alist k (1- oldval))
         alist)))

(define (increase! k alist)
   (let* ((oldval (assoc k alist)))
     (if oldval
         (assoc-set! alist k (1+ (cdr oldval)))
         (assoc-set! alist k 1))))

(define (one-step2 molecule rules)
 (dbg "molecule" molecule)
 (let* ((pairs (molecule2-pairs molecule))
        (first (molecule2-first molecule))
        (last (molecule2-last molecule)))
   (let loop ((pairs pairs) (result pairs))
    (begin
     (dbg "result" result)
     (if (null? pairs)
         (make-molecule2 result first last)
         (let* ((current (caar pairs))
                (nul (dbg "current" current))
                (rest (cdr pairs))
                (n (assoc current rules))
                (nul (dbg "n" n))
                (nul (dbg "rules" rules)))
           (if n
               (let* ((a (car current))
                      (b (cdr current))
                      (n (cdr n))
                      (result (decrease! current result))
                      (result (increase! (cons a n) result))
                      (result (increase! (cons n b) result)))
                  (loop (cdr pairs) result))
               (loop (cdr pairs) result))))))))


'( (((C . H) . B) ((H . H) . N) ((C . B) . H) ((N . H) . C) ((H . B) . C) ((H . C) . B) ((H . N) . C) ((N . N) . C) ((B . H) . H) ((N . C) . B) ((N . B) . B) ((B . N) . B) ((B . B) . N) ((B . C) . B) ((C . C) . N) ((C . N) . C)))

  
(define (iterate-polymerisation2 molecule rules times)
  (let loop ((times times) (molecule molecule))
    (begin
      (dbg "iterate molecule" molecule)
      (dbg "iterate times" times)
      (if (<= times 0)
          molecule
          (loop (1- times) (one-step2 molecule rules))))))

(define (iterate-polymerisation molecule rules times)
  (let loop ((times times) (molecule molecule))
      (if (<= times 0)
          molecule
          (loop (1- times) (one-step molecule rules)))))

(define (read-molecule port)
  (let* ((line (read-line port))
         (molecule (string->list line)))
    molecule))

(define (line->rule line)
   (let* ((line line)
          (separator (string-contains line " -> "))
          (pair (substring line 0 separator))
          (pair (cons (string-ref pair 0) (string-ref pair 1)))
          (inserted (string-ref line (+ 4 separator))))
     (cons pair inserted)))

(define (read-rules port)
  (let* ((bloc (read-bloc port))
         (rules (map line->rule bloc)))
      rules))

(define (count-elem list)
  (let loop ((list list) (res '()))
       (if (null? list) res
           (let* ((cur (car list))
                  (rest (cdr list))
                  (curcount (assoc cur res))
                  (curcount (if curcount (1+ (cdr curcount)) 1))
                  (res (assoc-set! res cur curcount)))
             (loop rest res)))))

(define (least-and-most list)
  (let* ((counts (count-elem list)))
     (let loop ((counts counts) (least #f) (most #f))
         (if (null? counts) (cons least most)
             (let* ((cur (car counts))
                    (rest (cdr counts))
                    (curcar (car cur))
                    (curcount (cdr cur))
                    (leastcount (if least (cdr least) 0))
                    (mostcount (if most (cdr most) 0))
                    (least (if (or (not least) (<= curcount leastcount)) cur least))
                    (most (if (>= curcount mostcount) cur most)))
                (loop rest least most))))))

(define (most-minus-least list)
  (let* ((l-and-m (least-and-most list))
         (lcount (cdar l-and-m))
         (mcount (cddr l-and-m)))
     (- mcount lcount)))

(define (count-pairs->count-chars pairs first last)
 (dbg "pairs" pairs)
 (let* ((char-count
         (let loop ((pairs pairs) (chars '()))
             (begin
              (if (null? pairs)
                  chars
                  (let* ((cur (car pairs))
                         (a (car cur))
                         (b (cdr cur))
                         (chars (increase! a chars))
                         (chars (increase! b chars))
                         (pairs (cdr pairs)))
                     (loop pairs chars))))))
        (char-count (map
                      (lambda (x) (cons (car x) (quotient (cdr x) 2)))
                      char-count))
        (char-count (increase! first char-count))
        (char-count (increase! last char-count)))
   char-count))

(define (molecule2->result molecule)
  (dbg "Xmolecule" molecule)
  (let* ((pairs (molecule2-pairs molecule))
         (first (molecule2-first molecule))
         (last  (molecule2-last molecule))
         (count-chars (count-pairs->count-chars pairs first last)))
     (most-minus-least count-chars)))

(define-public (main args)
  (let* ((port (current-input-port))
         (polymer (read-molecule port))
         (polymer2 (molecule->molecule2 polymer))
         (nul (dbg "polymer" polymer))
         (nul (dbg "polymer2" polymer2))
         (nul (read-line port))
         (rules (read-rules port))
         (step5 (iterate-polymerisation polymer rules 5))
         (step5-result (most-minus-least step5))
         (step5-2 (iterate-polymerisation2 polymer2 rules 5))
         (step5-result-2 (molecule2->result step5-2))
         (nul (dbg "step5-result" step5-result))
         (nul (dbg "step5-result-2" step5-result-2))
         (result1 "UNIMP")
         (result2 "UNIMP"))
    (format #t "result1: ~a\n" result1)
    (format #t "result2: ~a\n" result2)))
