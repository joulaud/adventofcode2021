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

(define-public (main args)
  (let* ((port (current-input-port))
         (polymer (read-molecule port))
         (nul (dbg "polymer" polymer))
         (nul (read-line port))
         (rules (read-rules port))
         (nul (dbg "polymer" polymer))
         (step1 (one-step polymer rules))
         (step2 (one-step step1 rules))
         (step3 (one-step step2 rules))
         (step4 (one-step step3 rules))
         (step5 (one-step step4 rules))
         (nul (dbg "step1" (list->string step1)))
         (nul (dbg "step2" (list->string step2)))
         (nul (dbg "step3" (list->string step3)))
         (nul (dbg "step4" (list->string step4)))
         (nul (dbg "step5" (list->string step5)))
         (step10 (iterate-polymerisation polymer rules 10))
         (nul (dbg "count" (count-elem step10)))
         (result1 (most-minus-least step10))
         (result2 "UNIMP"))
    (format #t "result1: ~a\n" result1)
    (format #t "result2: ~a\n" result2)))
