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
  (lambda (set) (= n (char-set-size set))))

(define (size-of-intersection-is set size)
  (lambda (x)
     (let ((common (char-set-intersection x set)))
       (= size (char-set-size common)))))

(define (pattern-list->map patterns)
  (let* ((sets  (map string->char-set patterns))
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
         (F     (char-set-intersection one six))
         ;; ZERO n'est ni NEUF ni SIX
         (zero  (car (filter (lambda (x) (and (not (equal? x nine)) (not (equal? x six)))) zero-six-nine)))
         ;; DEUX ne contient pas F (au contraire de TROIS et CINQ)
         (two   (car (filter (size-of-intersection-is F 0) two-three-five)))
         ;; TROIS a deux segments communs avec UN (C et F)
         (three (car (filter (size-of-intersection-is one 2) two-three-five)))
         ;; du coup CINQ est l'autre
         (five  (car (filter (lambda (x) (and (not (equal? x two)) (not (equal? x three)))) two-three-five)))
         (final-list (list
                       (cons (char-set->string zero)  0)
                       (cons (char-set->string one)   1)
                       (cons (char-set->string two)   2)
                       (cons (char-set->string three) 3)
                       (cons (char-set->string four)  4)
                       (cons (char-set->string five)  5)
                       (cons (char-set->string six)   6)
                       (cons (char-set->string seven) 7)
                       (cons (char-set->string eight) 8)
                       (cons (char-set->string nine)  9))))
   final-list))

(define (my-assoc alist key)
   ;; FIXME: canonicalize strings at parsing time to avoid sorting here
   (cdr
     (find
        (lambda(x) (string= (sort (car x) char<=?) (sort key char<=?)))
        alist)))
(define (count-1478 mapping lst)
  (fold
    (lambda (cur acc)
        (let ((num (my-assoc mapping cur)))
          (if (or
                (eq? num 1)
                (eq? num 4)
                (eq? num 7)
                (eq? num 8))
              (1+ acc)
              acc)))
    0
    lst))

(define (parse-input-line line)
   (let* ((x (string-split line #\|))
          (patterns (car x))
          (output (cadr x))
          (patterns (string-split patterns #\space))
          (patterns (filter (lambda (x) (not (string= "" x))) patterns))
          (output (string-split output #\space))
          (output (filter (lambda (x) (not (string= "" x))) output)))
    (cons patterns output)))

(define (count-all-1478 parsed-lines cur)
  (if (eqv? '() parsed-lines)
      cur
      (let* ((rest (cdr parsed-lines))
             (line (car parsed-lines))
             (mapping (car line))
             (output (cdr line))
             (count (count-1478 mapping output)))
         (count-all-1478 rest (+ cur count)))))


(define (output->number mapping output)
    (let loop ((rest output) (position 1000) (acc 0))
      (if (eqv? '() rest) acc
         (let ((digit (my-assoc mapping (car rest))))
           (loop (cdr rest) (/ position 10) (+ acc (* position digit)))))))

(define (all-outputs->number lst)
  (let loop ((lst lst) (acc 0))
      (if (eqv? '() lst)
          acc
          (let* ((rest (cdr lst))
                 (cur (car lst))
                 (curnum (output->number (car cur) (cdr cur))))
            (loop rest (+ acc curnum))))))

(define (parse-all port)
  (let loop ((cur '()))
      (let ((line (read-line port)))
        (if (eof-object? line)
            cur
            (let* ((line (parse-input-line line))
                   (patterns (car line))
                   (output (cdr line))
                   (mapping (pattern-list->map patterns)))
              (loop (cons (cons mapping output) cur)))))))

(define-public (main args)
  (let* (
         (parsed-lines (parse-all (current-input-port)))
         (result1 (count-all-1478 parsed-lines 0))
         (result2 (all-outputs->number parsed-lines)))
    (format #t "result1: ~a\n" result1)
    (format #t "result2: ~a\n" result2)))
