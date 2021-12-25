(define-module (adventofcode2021 day22 reactor)
    #:use-module (adventofcode2021 day22 utils)
    #:use-module (ice-9 rdelim)
    #:use-module (ice-9 match)
    #:use-module (ice-9 format)
    #:use-module (srfi srfi-1) ; lists library
    #:use-module (srfi srfi-2) ; and-let*
    #:use-module (srfi srfi-11) ; let*-values
    #:use-module (srfi srfi-9)
    #:use-module (srfi srfi-9 gnu) ; immutable records
    #:use-module (srfi srfi-41) ; Streams
    #:use-module (srfi srfi-43) ; Vectors iterators
    #:use-module (srfi srfi-26) ; cut (specializing parameters, currying alternative)
    #:use-module (srfi srfi-69)) ; hash tables

(use-modules (ice-9 rdelim))
(use-modules (ice-9 match))
(use-modules (ice-9 format))
(use-modules (ice-9 vlist))
(use-modules (srfi srfi-2)) ; and-let*
(use-modules (srfi srfi-1)) ; lists library
(use-modules (srfi srfi-11)) ; let*-values
(use-modules (srfi srfi-9))
(use-modules (srfi srfi-9 gnu)) ; immutable records
(use-modules (srfi srfi-41))
(use-modules (srfi srfi-26)) ; cut (specializing parameters, currying alternative)
(use-modules (srfi srfi-69)) ; hash tables

(define (dbg t v) (format #t "~a~a\n" t v) (force-output))
(define (dbgn t v) (format #t "~a~a  #  " t v) (force-output))

(define-immutable-record-type <cuboid>
  (make-cuboid x-min x-max y-min y-max z-min z-max on?)
  cuboid?
  (x-min cuboid-x-min set-cuboid-x-min)
  (x-max cuboid-x-max set-cuboid-x-max)
  (y-min cuboid-y-min set-cuboid-y-min)
  (y-max cuboid-y-max set-cuboid-y-max)
  (z-min cuboid-z-min set-cuboid-z-min)
  (z-max cuboid-z-max set-cuboid-z-max)
  (on? cuboid-on? set-cuboid-on-val))

(define (set-cuboid-on cuboid)
  (set-cuboid-on-val cuboid #t))

(define (set-cuboid-off cuboid)
  (set-cuboid-on-val cuboid #f))

(define (cuboid-not-empty? cuboid)
   (and (<= (cuboid-x-min cuboid) (cuboid-x-max cuboid))
        (<= (cuboid-y-min cuboid) (cuboid-y-max cuboid))
        (<= (cuboid-z-min cuboid) (cuboid-z-max cuboid))))

(define-record-type <reactor>
 (make-reactor-internal state cuboid)
 reactor?
 (state reactor-get-state! set-reactor-state!)
 (cuboid reactor-cuboid))

(define cuboid-50
    (make-cuboid -50 50
                 -50 50
                 -50 50
                 #f))

(define (make-reactor cuboid)
  (make-reactor-internal (make-hash-table)
                         cuboid-50))

(define (coord-list->cuboid lst)
  (make-cuboid (first  (first lst))
               (second (first lst))
               (first  (second lst))
               (second (second lst))
               (first  (third lst))
               (second (third lst))
               #t))

(define (coord-pairs->cuboid lst)
  (make-cuboid (car (first lst))
               (cdr (first lst))
               (car (second lst))
               (cdr (second lst))
               (car (third lst))
               (cdr (third lst))
               #t))

(define (parse-reboot-step line)
  (match-let* (((on? ranges) (string-split line #\space))
               (on?    (if (string=? on? "on") #t #f))
               (ranges (string-split ranges #\,))
               (ranges (map (cut substring <> 2) ranges)) ; get rid of "x=" prefix
               (ranges (map
                         (lambda (range)
                           (map string->number
                                (string-split-string range "..")))
                         ranges))
               (cuboid (coord-list->cuboid ranges)))
     (set-cuboid-on-val cuboid on?)))

(define (parse-all-reboot-steps port)
  (let parse-all-reboot-steps-rec ((line (read-line port))
                                   (steps '()))
      (cond
       ((eof-object? line) (reverse steps))
       (else
        (parse-all-reboot-steps-rec (read-line port)
                                    (cons (parse-reboot-step line)
                                          steps))))))

(define (axis-intersect x1-min x1-max x2-min x2-max)
  (cond
   ((or (< x1-max x2-min) (< x2-max x1-min))
    (values '()
            (list (cons x1-min x1-max))
            (list (cons x2-min x2-max))))
   ((and (= x1-min x2-min) (= x1-max x2-max))
    (values (list (cons x2-min      x1-max))
            '()
            '()))
   ((<= x1-min x2-min x1-max x2-max)
    (values (list (cons x2-min      x1-max))
            (list (cons x1-min      (1- x2-min)))
            (list (cons (1+ x1-max) x2-max))))
   ((<= x2-min x1-min x2-max x1-max)
    (values (list (cons x1-min      x2-max))
            (list (cons (1+ x2-max) x1-max))
            (list (cons x2-min      (1- x1-min)))))
   ((<= x1-min x2-min x2-max x1-max)
    (values (list (cons x2-min x2-max))
            (list (cons x1-min (1- x2-min)) (cons (1+ x2-max) x1-max))
            '()))
   ((<= x2-min x1-min x1-max x2-max)
    (values (list (cons x1-min x1-max))
            '()
            (list (cons x2-min (1- x1-min)) (cons (1+ x1-max) x2-max))))
   (else
    (values '() '() '()))))

(define (range-merge lst)
 ;; lst est une list de paires (min . max)
 (let ((lst (sort lst (lambda (a b) (or
                                      (< (car a) (car b))
                                      (and  (= (car a) (car b)) (< (cdr a) (cdr b))))))))
    (let range-merge-rec ((lst (cdr lst)) (result (list (car lst))))
            (cond
             ((null? lst) result)
             (else
              (let* ((rest (cdr lst))
                     (cur (car lst))
                     (cur-result (car result))
                     (rest-result (cdr result)))
                (cond
                 ((<= (car cur) (1+ (cdr cur-result)))
                  (range-merge-rec rest (cons (cons (car cur-result) (cdr cur))
                                              rest-result)))
                 (else
                  (range-merge-rec rest (cons cur result))))))))))

(define (ranges-only x-both y-both z-both x-only y-only z-only)
  (let* ((x-only+both (range-merge (append x-only x-both)))
         (z-only+both (range-merge (append z-only z-both)))
         (y-only+both (range-merge (append y-only y-both)))
         (ranges (append
                    (combine* x-only y-only+both z-only+both)
                    (combine* x-both y-only z-only+both)
                    (combine* x-both y-both z-only))))
         ;(_ (dbg "ranges=" ranges)))
     ranges))

(define (ranges->cuboids ranges)
  (let* ((cuboids (map coord-pairs->cuboid ranges))
         (cuboids (filter cuboid-not-empty? cuboids)))
    cuboids))

(define (cuboid-split-to-apply old reboot-step)
    (cond
     ((cuboid-on? reboot-step)
      (error "should never happen"))
     (else
        (let*-values (((x-both x-old x-new) (axis-intersect (cuboid-x-min old)
                                                            (cuboid-x-max old)
                                                            (cuboid-x-min reboot-step)
                                                            (cuboid-x-max reboot-step)))
                      ((y-both y-old y-new) (axis-intersect (cuboid-y-min old)
                                                            (cuboid-y-max old)
                                                            (cuboid-y-min reboot-step)
                                                            (cuboid-y-max reboot-step)))
                      ((z-both z-old z-new) (axis-intersect (cuboid-z-min old)
                                                            (cuboid-z-max old)
                                                            (cuboid-z-min reboot-step)
                                                            (cuboid-z-max reboot-step))))
           (let* ((ranges (ranges-only x-both y-both z-both x-old y-old z-old))
                  (cuboids (ranges->cuboids ranges)))
              cuboids)))))

(define (cuboid-included c1 c2)
   (and (>= (cuboid-x-min c1) (cuboid-x-min c2))
        (>= (cuboid-y-min c1) (cuboid-y-min c2))
        (>= (cuboid-z-min c1) (cuboid-z-min c2))
        (<= (cuboid-x-max c1) (cuboid-x-max c2))
        (<= (cuboid-y-max c1) (cuboid-y-max c2))
        (<= (cuboid-z-max c1) (cuboid-z-max c2))))

;(define (cuboid-adjacent c1 c2)
;   (and (= (cuboid-x-min c1) (cuboid-x-min c2))
;        (= (cuboid-x-max c1) (cuboid-x-max c2))
;        (= (cuboid-y-min c1) (cuboid-y-min c2))
;        (= (cuboid-y-max c1) (cuboid-y-max c2))))

(define (cuboid<? c1 c2)
  (or
   (< (cuboid-x-min c1) (cuboid-x-min c2))
   (and (= (cuboid-x-min c1) (cuboid-x-min c2))
        (< (cuboid-y-min c1) (cuboid-y-min c2)))
   (and (= (cuboid-x-min c1) (cuboid-x-min c2))
        (= (cuboid-y-min c1) (cuboid-y-min c2))
        (< (cuboid-z-min c1) (cuboid-z-min c2)))
   (and (= (cuboid-x-min c1) (cuboid-x-min c2))
        (= (cuboid-y-min c1) (cuboid-y-min c2))
        (= (cuboid-z-min c1) (cuboid-z-min c2))
        (< (cuboid-x-max c1) (cuboid-x-max c2)))
   (and (= (cuboid-x-min c1) (cuboid-x-min c2))
        (= (cuboid-y-min c1) (cuboid-y-min c2))
        (= (cuboid-z-min c1) (cuboid-z-min c2))
        (= (cuboid-x-max c1) (cuboid-x-max c2))
        (< (cuboid-y-max c1) (cuboid-y-max c2)))
   (and (= (cuboid-x-min c1) (cuboid-x-min c2))
        (= (cuboid-y-min c1) (cuboid-y-min c2))
        (= (cuboid-z-min c1) (cuboid-z-min c2))
        (= (cuboid-x-max c1) (cuboid-x-max c2))
        (= (cuboid-y-max c1) (cuboid-y-max c2))
        (< (cuboid-z-max c1) (cuboid-z-max c2)))))

(define (cuboids-apply-reboot-step cuboids reboot-step)
   (dbg "apply reboot-step: " (length cuboids))
   ;; On peut avoir des cuboïdes qui se recouvrent quand on applique
   ;; une reboot-step à "on" puisqu'on gère l'explosion des cuboïdes
   ;; indépendamment les uns des autres.
   (cond
    ((cuboid-on? reboot-step)
     (cons reboot-step cuboids))
    ((and (null? cuboids) (not (cuboid-on? reboot-step)))
     '())
    (else
     (append-map (cute cuboid-split-to-apply <> reboot-step)
                 cuboids))))

(define (cuboids-apply-all-reboot-steps cuboids reboot-steps)
  (cond
   ((null? reboot-steps)
    cuboids)
   (else
    ;(dbg "cuboids=" cuboids)
    (cuboids-apply-all-reboot-steps (cuboids-apply-reboot-step cuboids (car reboot-steps))
                                    (cdr reboot-steps)))))

(define (from-to a b)
  (if (<= a b)
      (iota (1+ (- b a)) a)
      '()))

(define (cuboid->cubes cuboid)
  ;; sort une list de cubes (chaque cube est une liste de coordonnées (list x y z)
  (let* ((r-x (from-to (cuboid-x-min cuboid) (cuboid-x-max cuboid)))
         (r-y (from-to (cuboid-y-min cuboid) (cuboid-y-max cuboid)))
         (r-z (from-to (cuboid-z-min cuboid) (cuboid-z-max cuboid))))
    (combine* r-x r-y r-z)))

(define (add-cuboid-to-hash-cubes! cubes cuboid)
   (let* ((new-cubes (cuboid->cubes cuboid)))
     (for-each
      (lambda (cube) (hash-table-set! cubes cube #t))
      new-cubes)))

(define (cuboids-to-hash-cubes cuboids)
   (let* ((cubes (make-hash-table)))
     (for-each
      (lambda (cuboid) (add-cuboid-to-hash-cubes! cubes cuboid))
      cuboids)
     cubes))

(define (cuboid-size cuboid)
    (let* ((x-size (max 0 (1+ (- (cuboid-x-max cuboid) (cuboid-x-min cuboid)))))
           (y-size (max 0 (1+ (- (cuboid-y-max cuboid) (cuboid-y-min cuboid)))))
           (z-size (max 0 (1+ (- (cuboid-z-max cuboid) (cuboid-z-min cuboid))))))
      (* x-size y-size z-size)))

(define (cuboid-intersect c1 c2)
    (let* ((x-min (max (cuboid-x-min c1) (cuboid-x-min c2)))
           (y-min (max (cuboid-y-min c1) (cuboid-y-min c2)))
           (z-min (max (cuboid-z-min c1) (cuboid-z-min c2)))
           (x-max (min (cuboid-x-max c1) (cuboid-x-max c2)))
           (y-max (min (cuboid-y-max c1) (cuboid-y-max c2)))
           (z-max (min (cuboid-z-max c1) (cuboid-z-max c2))))
      (make-cuboid x-min x-max y-min y-max z-min z-max (cuboid-on? c2))))

(define (cuboids-size cuboids)
  (fold
    (lambda (cuboid acc)
      ;(dbg "S,C=" (list (cuboid-size cuboid) cuboid))
      (+ acc (cuboid-size cuboid)))
    0
    cuboids))

(define (cuboids-size-bis cuboids)
    (hash-table-size (cuboids-to-hash-cubes cuboids)))

(define (delete-dups lst)
    (let delete-dups-rec ((lst (cdr lst)) (cur (car lst)) (result '()))
      (cond
       ((null? lst) (cons cur result))
       ((equal? (car lst) cur)
        (delete-dups-rec (cdr lst) cur result))
       (else
        (delete-dups-rec (cdr lst) (car lst) (cons cur result))))))

(define (sum-intersections-size cuboids-combinaisons)
    (let sum-intersections-rec ((rest cuboids-combinaisons) (size 0))
      (cond
       ((null? rest) size)
       (else
         (let* ((cur (car rest))
                (rest (cdr rest)))
           (cond
            ((equal? (first cur) (second cur)) (sum-intersections-rec rest size))
            (else (sum-intersections-rec
                   rest
                   (+ size (cuboids-size (cuboid-intersect (first cur) (second cur))))))))))))

(use-modules (statprof))
(define-public (main args)
   (let*-values (
                 ((steps) (parse-all-reboot-steps (current-input-port)))
                 ((steps-restricted) (map (cute cuboid-intersect cuboid-50 <>) steps))
                 ((cuboids-restricted) (cuboids-apply-all-reboot-steps '() steps-restricted))
                 ;(_ (statprof (lambda () (cuboids-apply-all-reboot-steps '() steps-restricted))))
                 ((size) (cuboids-size cuboids-restricted))
                 ((size-bis) (cuboids-size-bis cuboids-restricted))
                 ((result1) size-bis)
                 ((cuboids) (cuboids-apply-all-reboot-steps '() steps))
                 (_ (dbg "cuboids-length=" (length cuboids)))
                 ((cuboids) (delete-dups (sort cuboids cuboid<?)))
                 ((size) (cuboids-size cuboids))
                 (_ (dbg "cuboids-length=" (length cuboids)))
                 ((cuboids-combinaisons) (combine* cuboids cuboids))
                 (_ (dbg "combination length" (length cuboids-combinaisons)))
                 ((intersect-size) (sum-intersections-size cuboids-combinaisons))
                 ((result2) (- size intersect-size)))
      (format #t "result1: ~a\n" result1)
      (format #t "result2: ~a\n" result2)))
