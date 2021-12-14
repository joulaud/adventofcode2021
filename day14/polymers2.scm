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

(define (molecule2->string molecule)
  (let* ((pairs (molecule2-pairs molecule)))
    (pairs->string pairs)))

(define (pairs->string pairs)
      (fold
         (lambda (x acc)
           (let* ((pair (car x))
                  (val (cdr x))
                  (a (pair-a pair))
                  (b (pair-b pair)))
             (string-append
              acc (list->string (list a b))
              ": " (number->string val) "\n")))
         ""
         pairs))

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

(define (print-pairs text pairs)
  (format #t text)
  (for-each
    (lambda (x)
        (format #t "~a,~a: ~a\n" (pair-a (car x)) (pair-b (car x)) (cdr x)))
   (mysort pairs)))

(define (compare mol mol2)
  (let* (
         (mol (molecule->molecule2 mol))
         (pairs (molecule2-pairs mol))
         (pairs2 (molecule2-pairs mol2)))
     (for-each
        (lambda (pair)
            (let* ((val (cdr pair))
                   (pair (car pair))
                   (pair2 (or (assoc pair pairs2) (cons pair #f)))
                   (val2 (cdr pair2))
                   (pair (cons (pair-a pair) (pair-b pair))))
              (format #t "~a: ~a ~a\n" pair val val2)))
        pairs)))


(define (molecule->molecule2 molecule)
 (let* ((pairs (count-elem
                 (zip molecule (cdr molecule))))
        (pairs (map
                 (lambda (x) (cons (make-pair (caar x) (cadar x)) (cdr x)))
                 pairs))
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

;;(define (decrease! k alist num)
;;   (let* ((oldval (assoc k alist))
;;          (oldval (if oldval (cdr oldval) #f))
;;          (newval (if oldval (- oldval num) #f))
;;          (newval (if (>= newval 1) newval #f)))
;;     (if newval
;;         (assoc-set! alist k newval)
;;         (alist-delete! k alist))))

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

(define (iterate-polymerisation2 molecule rules times)
  (let loop ((times times) (molecule molecule))
    (begin
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

(define (rules->rules2 rules)
    (map
      (lambda (x)
        (begin
          (let* ((pair (car x))
                 (value (cdr x))
                 (a (car pair))
                 (b (cdr pair)))
            (cons (make-pair a b) value))))
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
 (let* ((char-count
         (let loop ((pairs pairs) (chars '()))
             (begin
              (if (null? pairs)
                  chars
                  (let* ((cur (car pairs))
                         (num (cdr cur))
                         (cur (car cur))
                         (a (pair-a cur))
                         (chars (increase! a chars num))
                         (pairs (cdr pairs)))
                     (loop pairs chars))))))
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
         (polymer (read-molecule port))
         (polymer2 (molecule->molecule2 polymer))
         (emptyline (read-line port))
         (rules (read-rules port))
         (rules2 (rules->rules2 rules))
         (polymer10 (iterate-polymerisation2 polymer2 rules2 10))
         (result1 (molecule2->result polymer10))
         (polymer40 (iterate-polymerisation2 polymer2 rules2 40))
         (result2 (molecule2->result polymer40)))
    (format #t "result1: ~a\n" result1)
    (format #t "result2: ~a\n" result2)))
