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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; disposition des sept segments        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   0:      1:      2:      3:      4:
;;  aaaa    ....    aaaa    aaaa    ....)
;; b    c  .    c  .    c  .    c  b    c
;; b    c  .    c  .    c  .    c  b    c
;;  ....    ....    dddd    dddd    dddd
;; e    f  .    f  e    .  .    f  .    f
;; e    f  .    f  e    .  .    f  .    f
;;  gggg    ....    gggg    gggg    ....
;;
;;   5:      6:      7:      8:      9:
;;  aaaa    aaaa    aaaa    aaaa    aaaa
;; b    .  b    .  .    c  b    c  b    c
;; b    .  b    .  .    c  b    c  b    c
;;  dddd    dddd    ....    dddd    dddd
;; .    f  e    f  .    f  e    f  .    f
;; .    f  e    f  .    f  e    f  .    f
;;  gggg    gggg    ....    gggg    gggg

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; liste des segments pour chaque num   ;;
;; trié par nombre de segments          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; '(1 . (2 . "CF"))       ; deux segments
;; '(7 . (3 . "ACF"))      ; trois segments
;; '(4 . (4 . "BCDF"))     ; 4 segments
;; '(2 . (5 . "ACDEG"))    ; ne contient pas F
;; '(3 . (5 . "ACDFG"))    ; C et F communs avec 1
;; '(5 . (5 . "ABDFG"))    ; ni 2 ni 3
;; '(0 . (6 . "ABCEFG"))   ; ni 6 ni 9
;; '(6.  (6 . "ABDEFG"))   ; seul F est commun avec 1
;; '(9 . (6 . "ABCDFG"))   ; BCDF communs avec 4
;; '(8 . (7 . "ABCDEFG"))  ; sept segments

;; On utilisera les char-sets de la SRFI-14 pour représenter l'afficheur

(define (size-is n)
  (lambda (pat)
     (= n (char-set-size (car pat)))))

(define (size-of-intersection-is pat size)
  (lambda (x)
     (let ((common (char-set-intersection (car x) (car pat))))
       (= size (char-set-size common)))))

(define (pattern-list->map patterns)
  (let* ((sets  (map (lambda (pat)
                         (cons (string->char-set pat) pat))
                     patterns))
         (one   (car (filter (size-is 2) sets))) ; seul UN a deux segments
         (seven (car (filter (size-is 3) sets))) ; seul SEPT a trois segments
         (four  (car (filter (size-is 4) sets))) ; seul QUATRE a quatre segments
         (eight (car (filter (size-is 7) sets))) ; seul HUIT a sept segments
         (two-three-five (filter (size-is 5) sets))
         (zero-six-nine (filter (size-is 6) sets))
         ;; ;; ZERO  et NEUF ont deux segments communs avec UN (C et F)
         ;; (zero-nine (filter (size-of-intersection-is one 2) zero-six-nine))
         ;; NEUF et QUATRE ont quatre segments communs
         (nine  (car (filter (size-of-intersection-is four 4) zero-six-nine)))
         ;; seul F est commun entre SIX et UN
         (six   (car (filter (size-of-intersection-is one 1) zero-six-nine)))
         (F     (cons (char-set-intersection (car one) (car six)) #f))
         ;; ZERO n'est ni NEUF ni SIX
         (zero  (car (filter (lambda (x) (and (not (equal? x nine)) (not (equal? x six)))) zero-six-nine)))
         ;; DEUX ne contient pas F (au contraire de TROIS et CINQ)
         (two   (car (filter (size-of-intersection-is F 0) two-three-five)))
         ;; TROIS a deux segments communs avec UN (C et F)
         (three (car (filter (size-of-intersection-is one 2) two-three-five)))
         ;; du coup CINQ est l'autre
         (five  (car (filter (lambda (x) (and (not (equal? x two)) (not (equal? x three)))) two-three-five)))
         (final-list (list
                       (cons (cdr zero)  0)
                       (cons (cdr one)   1)
                       (cons (cdr two)   2)
                       (cons (cdr three) 3)
                       (cons (cdr four)  4)
                       (cons (cdr five)  5)
                       (cons (cdr six)   6)
                       (cons (cdr seven) 7)
                       (cons (cdr eight) 8)
                       (cons (cdr nine)  9))))
   final-list))

(define-public (main args)
 (let* ((line (read-line))
        (lst (string-split line #\,))
        (numlst (map string->number lst))
        (result1 (min-fuel identity numlst))
        (metric (lambda (n) (/ (* n (1+ n)) 2)))
        (result2 (min-fuel metric numlst)))
    (format #t "result1: ~a\n" result1)
    (format #t "result2: ~a\n" result2)))
