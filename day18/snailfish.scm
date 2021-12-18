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

(define (snailfish-add a b)
  (cons a b))

(define-record-type <exploded>
  (make-exploded left right rest)
  exploded?
  (left exploded-left)
  (right exploded-right)
  (rest exploded-rest))

(define (snailfish-explode-internal l n)
    (let* ((left (car l))
           (right (cdr l)))
      (cond
       ((and (>= n 4)
             (number? left)
             (number? right))
        (make-exploded left right 0))
       ((and (number? left)
             (number? right))
        (make-exploded #f #f (cons left right)))
       ((number? left)
        (let* ((exploded (snailfish-explode-internal right (1+ n)))
               (explodedleft (exploded-left exploded))
               (explodedright (exploded-right exploded))
               (explodedrest (exploded-rest exploded)))
            (cond
             (explodedleft ;; on a un numéro à intégrer
              (let* ((newleft (+ left explodedleft))
                     (newrest (cons newleft explodedrest)))
                (make-exploded #f explodedright newrest)))
             (else
                (make-exploded #f explodedright (cons left explodedrest))))))
       ((number? right)
        (let* ((exploded (snailfish-explode-internal left (1+ n)))
               (explodedright (exploded-right exploded))
               (explodedleft (exploded-left exploded))
               (explodedrest (exploded-rest exploded)))
            (cond
             (explodedright ;; on a un numéro à intégrer
              (let* ((newright (+ right explodedright))
                     (newrest (cons explodedrest newright)))
                (make-exploded explodedleft #f newrest)))
             (else
                (make-exploded explodedleft #f (cons explodedrest right))))))
       (else
        (error "This is not a snailnumber: ~A" l)))))

(define (snailfish-explode l)
   (exploded-rest (snailfish-explode-internal l 0)))




(define (snailfish-split l)
   l)

(define (snailfish-reduce l)
   l)

(define-public (main args)
   (let* (
          (result1 "UNIMP")
          (result2 "UNIMP"))
     (format #t "result1: ~a\n" result1)
     (format #t "result2: ~a\n" result2)))
