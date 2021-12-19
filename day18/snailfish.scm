(define-module (adventofcode2021 day18 snailfish)
    #:use-module (adventofcode2021 day18 utils)
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
(use-modules (srfi srfi-2)) ; and-let*
(use-modules (srfi srfi-1)) ; lists library
(use-modules (srfi srfi-11)) ; let*-values
(use-modules (srfi srfi-9))
(use-modules (srfi srfi-9 gnu)) ; immutable records
(use-modules (srfi srfi-41))

(define (dbg t v) (format #t "~a~a\n" t v) (force-output))

(define (list->snailfish l)
  (let list->snailfish-rec ((l l))
     (cond
      ((null? l) (list 'CLOSE))
      ((number? l) (list l))
      (else
       (append
        (list 'OPEN)
        (list->snailfish-rec (car l))
        (list->snailfish-rec (cdr l))
        (list 'CLOSE))))))


(define (char->decimal c)
  (- (char->integer c) (char->integer #\0)))


(define (read-snailfish port)
  (define (read-snailfish-rec cur)
    (let ((c (read-char port)))
     (cond
      ((eof-object? c) (reverse cur))
      ((char=? c #\newline) (reverse cur))
      ((char=? c #\[) (read-snailfish-rec (cons 'OPEN cur)))
      ((char=? c #\]) (read-snailfish-rec (cons 'CLOSE cur)))
      ((char=? c #\,) (read-snailfish-rec cur))
      (else (read-rawnumber cur (char->decimal c))))))
  (define (read-rawnumber l num)
    (let ((c (read-char port)))
     (cond
      ((eof-object? c) (error "BAD INPUT"))
      ((char=? c #\newline) (error "BAD INPUT"))
      ((char=? c #\[) (error "BAD INPUT"))
      ((char=? c #\]) (read-snailfish-rec (cons* 'CLOSE num l)))
      ((char=? c #\,) (read-snailfish-rec (cons* num l)))
      (else (read-rawnumber l (+ (* 10 num) (char->decimal c)))))))
  (read-snailfish-rec '()))

(define (read-snailfishes port)
  (let read-snailfishes-rec ((lst '()))
    (let ((new (read-snailfish port)))
      (cond
       ((null? new) (reverse lst))
       (else (read-snailfishes-rec (cons new lst)))))))

(define (snailfish-add a b)
 ;; Warning: this addition is not commutative at all
 (cond
  ((and a b)
   (append '(OPEN) a b '(CLOSE)))
  (a a)
  (b b)
  (else #f)))

(define (begins-with-pair? l)
  (and
   (eq? 'OPEN (first l))
   (number? (second l))
   (number? (third l))
   (eq? 'CLOSE (fourth l))))

(define (add-to-next-regular-number l n)
  (let add-to-next-regular-number-rec
       ((l l) (before '()))
     (cond
      ((null? l) (reverse before))
      ((number? (car l))
       (append-reverse before (cons (+ n (car l))
                                    (cdr l))))
      (else
       (add-to-next-regular-number-rec (cdr l)
                                       (cons (car l) before))))))

(define (snailfish-explode-next-pair head tail)
  ;; We know here that tail begin with '(OPEN n1 n2 CLOSE)
  (let* ((n-left (second tail))
         (n-right (third tail))
         (tail (list-tail tail 4))
         (new-head (add-to-next-regular-number head n-left))
         (new-tail (add-to-next-regular-number tail n-right)))
     (append-reverse new-head (cons 0 new-tail))))

(define (snailfish-explode l)
    (let snailfish-explode-rec ((head '())
                                (tail l)
                                (depth 0))
        (cond
         ((null? tail) (cons #f (reverse head)))
         ((and (>= depth 4)
               (begins-with-pair? tail))
          (cons #t
                (snailfish-explode-next-pair head tail)))
         (else
          (snailfish-explode-rec
           (cons (car tail) head)
           (cdr tail)
           (cond
            ((eq? 'OPEN (car tail)) (1+ depth))
            ((eq? 'CLOSE (car tail)) (1- depth))
            (else depth)))))))

(define (snailfish-split l)
    (let snailfish-split-rec ((head '()) (tail l) (splitted? #f))
      (cond
       ((null? tail)
        (cons splitted? (reverse head)))
       (else
        (let* ((cur (car tail)))
          (cond
           ((and (number? cur) (>= cur 10))
            (let* ((n-left (quotient cur 2))
                   (n-right (+ n-left (remainder cur 2)))
                   (new-head (cons* 'CLOSE n-right n-left 'OPEN head)))
              (snailfish-split-rec new-head (cdr tail) #t)))
           (else
            (snailfish-split-rec (cons cur head) (cdr tail) splitted?))))))))

(define (snailfish-explode-all l)
   (let snailfish-explode-all-rec ((l l))
     (let* ((x (snailfish-explode l))
            (exploded? (car x))
            (result (cdr x)))
      (cond
       (exploded? (snailfish-explode-all-rec result))
       (else result)))))

(define (snailfish-reduce l)
   (let snailfish-reduce-rec ((l l))
      (let* ((exploded (snailfish-explode-all l))
             (splitted-res (snailfish-split exploded))
             (splitted? (car splitted-res))
             (result (cdr splitted-res)))
        (cond
         (splitted? (snailfish-reduce-rec result))
         (else result)))))

(define (snailfish-full-add a b)
  (let* ((sum (snailfish-add a b))
         (reduced-sum (snailfish-reduce sum)))
    reduced-sum))

(define (snailfish-full-add-lst lst)
  (fold
   (lambda (num prev)
       (snailfish-full-add prev num))
   #f
   lst))

(define-public (main args)
   (let* (
          (snailfishes (read-snailfishes (current-input-port)))
          (result1 snailfishes)
          (result2 "UNIMP"))
     (format #t "result1: ~a\n" result1)
     (format #t "result2: ~a\n" result2)))
