(define-module (first)
    #:use-module (ice-9 rdelim)
    #:use-module (ice-9 match)
    #:use-module (ice-9 format)
    #:use-module (srfi srfi-1) ; lists library
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

;; calculate fuel for position
(define (calc-fuel crabs position)
   (let* ((fuels (map
                   (lambda (crab-pos)
                      (abs (- position crab-pos)))
                   crabs))
          (total-fuel (fold
                        (lambda (fuel total-fuel)
                             (+ fuel total-fuel))
                       0
                       fuels)))
    total-fuel))

(define (min-max-pos crabs)
    (fold
       (lambda (crab cur)
          (if cur (cons (min crab (car cur)) (max crab (cdr cur)))
              (cons crab crab)))
       #f
       crabs))

;; from min-position to max-position calculate fuel and keep track of minimum fuel consumption
(define (min-fuel crabs)
  (let* ((min-max (min-max-pos crabs))
         (min-pos (car min-max))
         (max-pos (cdr min-max)))
    (let loop ((cur-pos min-pos) (min-fuel #f))
          (if (> cur-pos max-pos) min-fuel
              (let ((cur-fuel (calc-fuel crabs cur-pos)))
                 (if min-fuel
                     (loop (1+ cur-pos) (min min-fuel cur-fuel))
                     (loop (1+ cur-pos) cur-fuel)))))))

(define-public (main args)
  (let* ((line (read-line))
         (lst (string-split line #\,))
         (numlst (map string->number lst))
         (result (min-fuel numlst)))
     (format #t "result: ~a\n" result)))
