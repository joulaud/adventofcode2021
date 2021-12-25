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
    #:use-module (srfi srfi-69) ; hash tables
    #:use-module (srfi srfi-171)) ; transducers

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
      (lambda (cuboid) (if (cuboid-on? cuboid) (add-cuboid-to-hash-cubes! cubes cuboid)))
      cuboids)
     cubes))

(define (cuboid-size cuboid)
  (cond
   ((cuboid-on? cuboid)
    (let* ((x-size (max 0 (1+ (- (cuboid-x-max cuboid) (cuboid-x-min cuboid)))))
           (y-size (max 0 (1+ (- (cuboid-y-max cuboid) (cuboid-y-min cuboid)))))
           (z-size (max 0 (1+ (- (cuboid-z-max cuboid) (cuboid-z-min cuboid)))))
           (cuboid-size (* x-size y-size z-size)))
       cuboid-size))
   (else 0)))

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
 (cond
  ((<= (length lst) 1)
   lst)
  (else
    (let delete-dups-rec ((lst (cdr lst)) (cur (car lst)) (result '()))
      (cond
       ((null? lst) (cons cur result))
       ((equal? (car lst) cur)
        (delete-dups-rec (cdr lst) cur result))
       (else
        (delete-dups-rec (cdr lst) (car lst) (cons cur result))))))))

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
                   (+ size (cuboid-size (cuboid-intersect (first cur) (second cur))))))))))))

(define (cuboid-full-split c1 c2)
        (let*-values (((x-both x-c1 x-c2) (axis-intersect (cuboid-x-min c1)
                                                          (cuboid-x-max c1)
                                                          (cuboid-x-min c2)
                                                          (cuboid-x-max c2)))
                      ((y-both y-c1 y-c2) (axis-intersect (cuboid-y-min c1)
                                                          (cuboid-y-max c1)
                                                          (cuboid-y-min c2)
                                                          (cuboid-y-max c2)))
                      ((z-both z-c1 z-c2) (axis-intersect (cuboid-z-min c1)
                                                          (cuboid-z-max c1)
                                                          (cuboid-z-min c2)
                                                          (cuboid-z-max c2))))
           (let* ((ranges  (append
                            (ranges-only x-both y-both z-both x-c1 y-c1 z-c1)
                            (ranges-only x-both y-both z-both x-c2 y-c2 z-c2)
                            (combine* x-both y-both z-both)))
                  (cuboids (ranges->cuboids ranges)))
              cuboids)))

(define (normalize-cuboids cuboids)
  (dbg "Ncuboids-length=" (length cuboids))
  (cond
   ((<= (length cuboids) 1)
    cuboids)
   (else
    (let* (
           (cub-zip (zip cuboids (cdr cuboids)))
           (new-cuboids (fold
                          (lambda (x acc)
                               (append (cuboid-full-split (first x) (second x)) acc))
                          '()
                          cub-zip))
           (new-cuboids (delete-dups (sort new-cuboids cuboid<?))))
      (cond
       ((equal? cuboids new-cuboids) cuboids)
       (else (normalize-cuboids new-cuboids)))))))

(define (cuboid->range-tree cuboid)
    (let* ((x-range (cons (cuboid-x-min cuboid) (cuboid-x-max cuboid)))
           (y-range (cons (cuboid-y-min cuboid) (cuboid-y-max cuboid)))
           (z-range (cons (cuboid-z-min cuboid) (cuboid-z-max cuboid))))
     (list
      (cons x-range
            (list (cons y-range
                        (list z-range)))))))

(define (delete-adjacent-duplicates lst)
 (cond
  ((null? lst)
   lst)
  (else
   (reverse
          (fold
            (lambda (elem res)
              (if (equal? elem (first res))
                  res
                  (cons elem res)))
            (list (first lst))
            lst)))))


(define (cuboids->all-x cuboids)
    (let loop ((cuboids cuboids) (result '()))
       (cond
        ((null? cuboids)
         (delete-adjacent-duplicates (sort result <)))
        (else (let* ((cur (car cuboids))
                     (rest (cdr cuboids))
                     (x-min (cuboid-x-min cur))
                     (x-max (1+ (cuboid-x-max cur)))
                     (result (cons x-min (cons x-max result))))
                 (loop rest result))))))

(define (cuboids->all-y cuboids)
    (let loop ((cuboids cuboids) (result '()))
       (cond
        ((null? cuboids)
         (delete-adjacent-duplicates (sort result <)))
        (else (let* ((cur (car cuboids))
                     (rest (cdr cuboids))
                     (y-min (cuboid-y-min cur))
                     (y-max (1+ (cuboid-y-max cur)))
                     (result (cons y-min (cons y-max result))))
                 (loop rest result))))))

(define (cuboids->all-z cuboids)
    (let loop ((cuboids cuboids) (result '()))
       (cond
        ((null? cuboids)
         (delete-adjacent-duplicates (sort result <)))
        (else (let* ((cur (car cuboids))
                     (rest (cdr cuboids))
                     (z-min (cuboid-z-min cur))
                     (z-max (1+ (cuboid-z-max cur)))
                     (result (cons z-min (cons z-max result))))
                 (loop rest result))))))

(define (cuboid-restrict-x c min-x max-x)
 (let* ((c (set-cuboid-x-min c (max (cuboid-x-min c) min-x)))
        (c (set-cuboid-x-max c (min (cuboid-x-max c) max-x))))
   c))

(define (cuboid-restrict-y c min-y max-y)
 (let* ((c (set-cuboid-y-min c (max (cuboid-y-min c) min-y)))
        (c (set-cuboid-y-max c (min (cuboid-y-max c) max-y))))
   c))

(define (cuboid-restrict-z c min-z max-z)
 (let* ((c (set-cuboid-z-min c (max (cuboid-z-min c) min-z)))
        (c (set-cuboid-z-max c (min (cuboid-z-max c) max-z))))
    c))

(define (cuboids-restricted-to-x-range cuboids min-x max-x)
    (filter
      cuboid-not-empty?
      (map
        (cut cuboid-restrict-x <> min-x max-x)
        cuboids)))

(define (cuboids-restricted-to-y-range cuboids min-y max-y)
    (filter
      cuboid-not-empty?
      (map
        (cut cuboid-restrict-y <> min-y max-y)
        cuboids)))

(define (cuboids-restricted-to-z-range cuboids min-z max-z)
    (filter
      cuboid-not-empty?
      (map
        (cut cuboid-restrict-z <> min-z max-z)
        cuboids)))

(define (all-subintervals lst)
 (cond
  ((null? lst) lst)
  (else (let* ((v1 (first lst))
               (mins (map 1+ lst))
               (mins (cons v1 (cdr mins))))
          (zip mins (cdr lst))))))

(define (cuboids-normalise-by-axis axis f-all-n f-restrict f-next-step)
  (lambda (cuboids)
    (let*  ((all-n (f-all-n cuboids))
            (n-ranges (all-subintervals all-n))
            (cuboids (map
                          (lambda (x)
                             (let* ((n-min (first x))
                                    (n-max (second x))
                                    (this-cuboids (f-restrict cuboids n-min n-max))
                                    (this-cuboids (delete-duplicates (f-next-step this-cuboids))))
                                 this-cuboids))
                          n-ranges)))
        (concatenate cuboids))))

(define (cuboids-normalise-ter cuboids)
    (let*  ((all-x (cuboids->all-x cuboids))
            (all-x (zip all-x (map 1- (cdr all-x))))
            (all-y (cuboids->all-y cuboids))
            (all-y (zip all-y (map 1- (cdr all-y))))
            (all-z (cuboids->all-z cuboids))
            (all-z (zip all-z (map 1- (cdr all-z))))
            (_ (dbg "intervals=" (list all-x all-y all-z)))
            (cuboids (fold-three-combinations
                       (lambda (x y z acc)
                         (let ((c (coord-list->cuboid (list x y z))))
                           (cond
                            ((any (cut cuboid-included c <>) cuboids)
                             (cons c acc))
                            (else acc))))
                       '()
                       all-x
                       all-y
                       all-z)))
      cuboids))

(define cuboids-normalise-z-range
  (cuboids-normalise-by-axis 'z cuboids->all-z cuboids-restricted-to-z-range identity))

(define cuboids-normalise-y-range
  (cuboids-normalise-by-axis 'y cuboids->all-y cuboids-restricted-to-y-range cuboids-normalise-z-range))

(define cuboids-normalise-x-range
  (cuboids-normalise-by-axis 'x cuboids->all-x cuboids-restricted-to-x-range cuboids-normalise-y-range))

(define (old-cuboids-normalise-x-range cuboids)
    (let* ((all-x (cuboids->all-x cuboids))
           (x-ranges (all-subintervals all-x))
           (cuboids (map
                         (lambda (x)
                            (let* ((x-min (first x))
                                   (x-max (second x))
                                   (this-cuboids (cuboids-restricted-to-x-range cuboids x-min x-max))
                                   (this-cuboids (cuboids-normalise-y-range this-cuboids)))
                                this-cuboids))
                         x-ranges)))
        (concatenate cuboids)))

(define (old-cuboids-normalise-y-range cuboids)
    (let* ((all-y (cuboids->all-y cuboids))
           (y-ranges (all-subintervals all-y))
           (cuboids (map
                         (lambda (x)
                            (let* ((y-min (first x))
                                   (y-max (second x))
                                   (this-cuboids (cuboids-restricted-to-y-range cuboids y-min y-max))
                                   (this-cuboids (cuboids-normalise-z-range this-cuboids)))
                                this-cuboids))
                         y-ranges)))
        (concatenate cuboids)))

(define (old-cuboids-normalise-z-range cuboids)
    (let* ((all-z (cuboids->all-z cuboids))
           (z-ranges (all-subintervals all-z))
           (cuboids (map
                         (lambda (x)
                            (let* ((z-min (first x))
                                   (z-max (second x))
                                   (this-cuboids (cuboids-restricted-to-z-range cuboids z-min z-max)))
                                this-cuboids))
                         z-ranges)))
        (concatenate cuboids)))
    
(use-modules (statprof))
(define-public (main args)
   (let*-values (
                 ((steps) (parse-all-reboot-steps (current-input-port)))
                 ((steps-restricted) (map (cute cuboid-intersect cuboid-50 <>) steps))
                 ((cuboids-restricted) (cuboids-apply-all-reboot-steps '() steps-restricted))
                 (_ (dbg "L=" (length cuboids-restricted)))
                 ((cuboids-restricted) (cuboids-normalise-ter cuboids-restricted))
                 ((size) (cuboids-size cuboids-restricted))
                 ((size-bis) (cuboids-size-bis cuboids-restricted))
                 (_ (dbg "s,S,L=" (list size size-bis (length cuboids-restricted))))
                 ((result1) size-bis)
                 ((cuboids) (cuboids-apply-all-reboot-steps '() steps))
                 (_ (dbg "L=" (length cuboids)))
                 (_ (statprof (lambda () (cuboids-normalise-ter cuboids))))
                 (_ (exit))
                 ((cuboids) (cuboids-normalise-ter cuboids))
                 (_ (dbg "L=" (length cuboids)))
                 ((result2) (cuboids-size cuboids)))
      (format #t "result1: ~a\n" result1)
      (format #t "result2: ~a\n" result2)))

