(define-module (adventofcode2021 day14 polymers2)
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

(define (dbg t v) (format #t "~a~a\n" t v) (force-output))

(define-record-type <molecule2>
   (make-molecule2 pairs first last)
   molecule2?
   (pairs molecule2-pairs)
   (first molecule2-first)
   (last molecule2-last))
(define-record-type <pair>
  (make-pair-internal l)
  pair?
  (l pair-l))

(define (make-pair a b)
  (make-pair-internal (list->string (list a b))))

(define (pair-a pair)
  (string-ref (pair-l pair) 0))

(define (pair-b pair)
  (string-ref (pair-l pair) 1))

(define (molecule->molecule2 molecule)
 (let* ((pairs (count-elem
                 (zip molecule (cdr molecule))))
        (pairs (map
                 (lambda (x) (cons (make-pair (caar x) (cadar x)) (cdr x)))
                 pairs))
        (last (list-ref molecule (1- (length molecule)))))
   (make-molecule2 pairs (car molecule) last)))

(define (increase! k alist num)
   (let* ((oldval (assoc k alist)))
     (if oldval
         (assoc-set! alist k (+ num (cdr oldval)))
         (assoc-set! alist k num))))

(define (one-step2 molecule rules)
 (let* ((pairs (molecule2-pairs molecule))
        (first (molecule2-first molecule))
        (last (molecule2-last molecule)))
   (let loop ((pairs pairs) (result '()))
     (if (null? pairs)
         (make-molecule2 result first last)
         (let* ((current (car pairs))
                (val (cdr current))
                (current (car current))
                (rest (cdr pairs))
                (n (assoc current rules)))
           (if n
               (let* ((a (pair-a current))
                      (b (pair-b current))
                      (n (cdr n))
                      (result (increase! (make-pair a n) result val))
                      (result (increase! (make-pair n b) result val)))
                  (loop (cdr pairs) result))
               (loop (cdr pairs) result)))))))

(define (read-molecule port)
  (let* ((line (read-line port))
         (molecule (string->list line)))
    molecule))

(define (read-molecule2 port)
 (let* ((line (read-line port))
        (chars (string->list line))
        (charpairs (zip chars (cdr chars)))
        (countedpairs (fold
                        (lambda (x acc)
                           (increase! (make-pair (car x) (cadr x)) acc 1))
                        '()
                        charpairs))
        (last (list-ref chars (1- (length chars))))
        (first (car chars)))
   (make-molecule2 countedpairs first last)))

(define (line->rule2 line)
   (let* ((line line)
          (separator (string-contains line " -> "))
          (pair (substring line 0 separator))
          (pair (make-pair-internal pair))
          (inserted (string-ref line (+ 4 separator))))
     (cons pair inserted)))

(define (read-rules port)
  (let* ((bloc (read-bloc port))
         (rules (map line->rule2 bloc)))
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
             (let* ((cur        (car counts))
                    (rest       (cdr counts))
                    (curcar     (car cur))
                    (curcount   (cdr cur))
                    (leastcount (if least (cdr least) 0))
                    (mostcount  (if most (cdr most) 0))
                    (least      (if (or (not least) (<= curcount leastcount)) cur least))
                    (most       (if (>= curcount mostcount) cur most)))
                (loop rest least most))))))

(define (most-minus-least list)
  (let* ((l-and-m (least-and-most list))
         (lcount (cdar l-and-m))
         (mcount (cddr l-and-m)))
     (- mcount lcount)))

(define (count-pairs->count-chars pairs first last)
 (let* ((char-count (fold
                      ;; count left char of each pair
                      (lambda (x acc) (increase! (pair-a (car x)) acc (cdr x)))
                      '()
                      pairs))
         ;; count last char as it is on left on no pair
        (char-count (increase! last char-count 1)))
   char-count))

(define (molecule2->result molecule)
  (let* ((pairs (molecule2-pairs molecule))
         (first (molecule2-first molecule))
         (last  (molecule2-last molecule))
         (count-chars (count-pairs->count-chars pairs first last))
         (count-chars (sort count-chars
                            (lambda (x y) (<= (cdr x) (cdr y))))))
    (- (cdr (list-ref count-chars (1- (length count-chars))))
       (cdar count-chars))))

(define-public (main args)
  (let* ((port (current-input-port))
         (polymer2 (read-molecule2 port))
         (emptyline (read-line port))
         (rules2 (read-rules port))
         (polymer10 (fold
                      (lambda (_ polymer2) (one-step2 polymer2 rules2))
                      polymer2
                      (iota 10)))
         (result1 (molecule2->result polymer10))
         (polymer40 (fold
                      (lambda (_ polymer2) (one-step2 polymer2 rules2))
                      polymer10
                      (iota 30)))
         (result2 (molecule2->result polymer40)))
    (format #t "result1: ~a\n" result1)
    (format #t "result2: ~a\n" result2)))
