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

(define (new-lanterfish-school)
  ;; A lanterfish School is comprised of lanterfishes at different stages
  ;; of their reproduction cycle
  ;; This is a vector with the number of lanternfishes in each stage.
  (make-vector 9 0))

(define (vector-inc! vec i inc)
  (let* ((cur (vector-ref vec i)))
    (vector-set! vec i (+ cur inc))
    vec))

(define (one-day school)
  (let ((new-school (new-lanterfish-school)))
    (vector-for-each
       (lambda (i num)
          (if (eq? i 0)
              (begin
                  ;; reproduction time
                  (vector-inc! new-school 8 num)
                  (vector-inc! new-school 6 num))
              (vector-inc! new-school (- i 1) num)))
       school)
    new-school))

(define (iterate school numdays)
  (let loop ((oldschool school) (numdays numdays))
     (if (<= numdays 0) oldschool
         (loop (one-day oldschool) (1- numdays)))))

(define (port->school port)
   (let* ((line (read-line port))
          (list (string-split line #\,))
          (numlist (map string->number list))
          (school (new-lanterfish-school)))
     (fold
        (lambda (num school)
          (vector-inc! school num 1))
        school
        numlist)))

(define (count-lanternfishes school)
 (vector-fold
    (lambda (i acc num)
      (+ acc num))
    0
    school))

(define-public (main args)
  (let* ((lanternfish-school (port->school (current-input-port)))
         (after-80days (count-lanternfishes (iterate lanternfish-school 80)))
         (after-256days (count-lanternfishes (iterate lanternfish-school 256))))
     (format #t "result: ~a\n" after-80days)
     (format #t "result: ~a\n" after-256days)))
