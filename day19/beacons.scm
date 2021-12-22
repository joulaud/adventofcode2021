(define-module (adventofcode2021 day19 beacons)
    #:use-module (adventofcode2021 day19 utils)
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
    #:use-module (srfi srfi-26)) ; cut (specializing parameters, currying alternative)

(use-modules (ice-9 rdelim))
(use-modules (ice-9 match))
(use-modules (ice-9 format))
(use-modules (srfi srfi-2)) ; and-let*
(use-modules (srfi srfi-1)) ; lists library
(use-modules (srfi srfi-11)) ; let*-values
(use-modules (srfi srfi-9))
(use-modules (srfi srfi-9 gnu)) ; immutable records
(use-modules (srfi srfi-41))
(use-modules (srfi srfi-26)) ; cut (specializing parameters, currying alternative)


(define (dbg t v) (format #t "~a~a\n" t v) (force-output))
(define (dbgn t v) (format #t "~a~a  #  " t v) (force-output))
(define (dbgc t v) (format #t "~a~a\n" t (coord->string v)) (force-output))
(define (dstpair->print p)
  (cond
   (p (cons (coord->string (car p)) (cons (coord->string (cadr p)) (coord->string (cddr p)))))
   (else #f)))

(define-record-type <coord>
  (make-coord x y z)
  coord?
  (x coord-x)
  (y coord-y)
  (z coord-z))

(define (coord=? a b)
  (and
   (= (coord-x a) (coord-x b))
   (= (coord-y a) (coord-y b))
   (= (coord-z a) (coord-z b))))

(define (coord<? a b)
  (cond
   ((< (coord-x a) (coord-x b)) #t)
   ((and
     (= (coord-x a) (coord-x b))
     (< (coord-y a) (coord-y b)))
    #t)
   ((and
     (= (coord-x a) (coord-x b))
     (= (coord-y a) (coord-y b))
     (< (coord-z a) (coord-z b)))
    #t)
   (else #f)))

(define (coord<=? a b)
  (cond
   ((< (coord-x a) (coord-x b)) #t)
   ((and
     (= (coord-x a) (coord-x b))
     (< (coord-y a) (coord-y b)))
    #t)
   ((and
     (= (coord-x a) (coord-x b))
     (= (coord-y a) (coord-y b))
     (<= (coord-z a) (coord-z b)))
    #t)
   (else #f)))

(define (mymin f args)
  (let mymin-rec ((args args)(result #f))
    (cond
     ((null? args) result)
     (else
      (let* ((less? (if result (f (car args) result) #t))
             (new-min (if less? (car args) result)))
       (mymin-rec (cdr args) new-min))))))

(define (coord->string c)
 (cond
  ((eq? 'nil c) "(nil)")
  (else (string-append
         "("
         (number->string (coord-x c))
         ", "
         (number->string (coord-y c))
         ", "
         (number->string (coord-z c))
         ")"))))

(define-record-type <scan>
  (make-scan-int beacons pairs name)
  scan?
  (beacons scan-beacons)
  (pairs scan-pairs)
  (name scan-name))

(define (make-scan beacons name)
  (let ((pairs (scan-beacons->pairs-by-distances beacons)))
    (make-scan-int beacons pairs name)))

(define (scan->string scan)
  (fold
   (lambda (coord acc)
     (string-append
      (coord->string coord)
      "\n"
      acc))
   "\n----\n"
   (scan-beacons scan)))

(define (scan-length scan)
  (length (delete-duplicates (scan-beacons scan))))

(define (string->coord str)
  (let* ((x (string-split str #\,))
         (x (map string->number x)))
    (make-coord (first x) (second x) (third x))))

(define (read-scanner port)
  (let read-scanner-rec ((beacons '())(name "UNK"))
    (let* ((line (read-line port)))
      (cond
       ((eof-object? line) (cons (make-scan (reverse beacons) name) line)) ; end-of-file
       ((string-null? line) (cons (make-scan (reverse beacons) name) line)) ; end-of-paragraph
       ((string-prefix? "--- scanner" line) (read-scanner-rec beacons (string-drop-right (string-drop line 12) 4))) ; begin of next paragraph
       (else
        (read-scanner-rec
          (cons (string->coord line) beacons)
          name))))))

(define (read-all-scanners port)
  (let read-all-scanners-rec ((scanners '()))
    (let ((scan (read-scanner port)))
      (cond
        ((eof-object? (cdr scan)) (reverse (cons (car scan) scanners)))
        (else (read-all-scanners-rec (cons (car scan) scanners)))))))
(define (distance a b)
  (let* ((dx (abs (- (coord-x a) (coord-x b))))
         (dy (abs (- (coord-y a) (coord-y b))))
         (dz (abs (- (coord-z a) (coord-z b))))
         (sorted (sort (list dx dy dz) <)))
     (apply make-coord sorted)))

(define (pairs elem l)
  (let pairs-rec ((rest l) (result '()))
     (cond
      ((null? rest) result)
      (else (pairs-rec (cdr rest) (cons
                                   (cons elem (car rest))
                                   result))))))

(define (allpairs l)
  (let allpairs-rec ((rest l) (result '()))
     (cond
      ((null? rest) result)
      (else (allpairs-rec (cdr rest)
                          (append-reverse (pairs (car rest) (cdr rest)) result))))))

(define (scan->pairs-by-distances scan)
   (scan-beacons->pairs-by-distances (scan-beacons scan)))

(define (scan-beacons->pairs-by-distances scan)
 (let* (
        (x (allpairs scan))
        (x (fold
             (lambda (pair acc)
                 (acons
                  (distance (car pair) (cdr pair))
                  pair
                  acc))
             '()
             x))
        (x (sort
             x
             (lambda (a b) (coord<? (car a) (car b))))))
    x))

(define (get-transforms-candidates from target)
    ;; Si CURRENT et TARGET représentent le même point dans deux
    ;; référentiels différents S1 et S0, alors on retourne la liste
    ;; des transformations possibles composées d'une parmi les 24 rotations
    ;; et de la translation correspondant pour transformer toute
    ;; coordonnée de S1 dans le référentiel de S0.
    (map
      (lambda (rotation)
          (let* (
                 (from (rotation from))
                 (delta-x (- (coord-x target) (coord-x from)))
                 (delta-y (- (coord-y target) (coord-y from)))
                 (delta-z (- (coord-z target) (coord-z from))))
            (lambda (coord)
               (let* ((rotated (rotation coord)))
                 (make-coord
                  (+ delta-x (coord-x rotated))
                  (+ delta-y (coord-y rotated))
                  (+ delta-z (coord-z rotated)))))))
     all-rotations-func))

(define (transform->string transform)
  (string-concatenate
   (map
      (lambda (c)
       (coord->string
         (transform
           (apply make-coord c))))
     '(( 1 0 0) (0 1 0) (0 0 1)))))


(define (is-possible-transform? target-pair pair transform)
  (let* ((target-a (car target-pair))
         (target-b (cdr target-pair))
         (a (transform (car pair)))
         (b (transform (cdr pair)))
         (result (or
                  (and
                   (coord=? a target-a)
                   (coord=? b target-b))
                  (and
                   (coord=? a target-b)
                   (coord=? b target-a)))))
    result))

(define (get-first-pair-with-same-distance dstpairs1 dstpairs0)
   (let loop ((dstpairs1 dstpairs1))
     (cond
      ((null? dstpairs1) #f)
      (else
       (let* ((cur (car dstpairs1))
              (curdst (car cur))
              (res (assoc curdst dstpairs0)))
         (cond
          (res curdst)
          (else (loop (cdr dstpairs1)))))))))

(define (transform-equal? a b)
  (string=? (transform->string a) (transform->string b)))

(define (all-transform-equals? lst)
  (reduce
       (lambda (a b)
          (if (transform-equal? a b) a #f))
      #t
      lst))



;; autour de z
;;   0: x=x  y=y  z=z
;;  90: x=y  y=-x z=z
;; 180: x=-x y=-y z=z
;; 270: x=-y y=x  z=z
;; autour de y
;;   0: x=x  y=y z=z
;;  90: x=z  y=y z=-x
;; 180: x=-x y=y z=-z
;; 270: x=-z y=y z=x
;; autour de x
;;   0: x=x y=y  z=z
;;  90: x=x y=-z z=y
;; 180: x=x y=-y z=-z
;; 270: x=x y=z  z=-y
;; autour de -z
;;   0: x=x  y=y  z=-z
;;  90: x=y  y=-x z=-z
;; 180: x=-x y=-y z=-z
;; 270: x=-y y=x  z=-z
;; autour de -y
;;   0: x=x  y=-y z=z
;;  90: x=z  y=-y z=-x
;; 180: x=-x y=-y z=-z
;; 270: x=-z y=-y z=x
;; autour de -x
;;   0: x=-x y=y  z=z
;;  90: x=-x y=-z z=y
;; 180: x=-x y=-y z=-z
;; 270: x=-x y=z  z=-y

(define rotation-zP0     (lambda (c) (make-coord (+ (coord-x c))  (+ (coord-y c)) (+ (coord-z c)))))
(define rotation-zP90   (lambda (c) (make-coord (+ (coord-y c))  (- (coord-x c)) (+ (coord-z c)))))
(define rotation-zP180  (lambda (c) (make-coord (- (coord-x c))  (- (coord-y c)) (+ (coord-z c)))))
(define rotation-zP270  (lambda (c) (make-coord (- (coord-y c))  (+ (coord-x c)) (+ (coord-z c)))))

(define rotation-yP0    (lambda (c) (make-coord (+ (coord-x c))  (+ (coord-y c)) (+ (coord-z c)))))
(define rotation-yP90   (lambda (c) (make-coord (+ (coord-z c))  (+ (coord-y c)) (- (coord-x c)))))
(define rotation-yP180  (lambda (c) (make-coord (- (coord-x c))  (+ (coord-y c)) (- (coord-z c)))))
(define rotation-yP270  (lambda (c) (make-coord (- (coord-z c))  (+ (coord-y c)) (+ (coord-x c)))))

(define rotation-xP0    (lambda (c) (make-coord (+ (coord-x c))  (+ (coord-y c)) (+ (coord-z c)))))
(define rotation-xP90   (lambda (c) (make-coord (+ (coord-x c))  (- (coord-z c)) (+ (coord-y c)))))
(define rotation-xP180  (lambda (c) (make-coord (+ (coord-x c))  (- (coord-y c)) (- (coord-z c)))))
(define rotation-xP270  (lambda (c) (make-coord (+ (coord-x c))  (+ (coord-z c)) (- (coord-y c)))))

(define rotation-zN0    (lambda (c) (make-coord (+ (coord-x c))  (+ (coord-y c)) (- (coord-z c)))))
(define rotation-zN90   (lambda (c) (make-coord (+ (coord-y c))  (- (coord-x c)) (- (coord-z c)))))
(define rotation-zN180  (lambda (c) (make-coord (- (coord-x c))  (- (coord-y c)) (- (coord-z c)))))
(define rotation-zN270  (lambda (c) (make-coord (- (coord-y c))  (+ (coord-x c)) (- (coord-z c)))))

(define rotation-yN0    (lambda (c) (make-coord (+ (coord-x c))  (- (coord-y c)) (+ (coord-z c)))))
(define rotation-yN90   (lambda (c) (make-coord (+ (coord-z c))  (- (coord-y c)) (- (coord-x c)))))
(define rotation-yN180  (lambda (c) (make-coord (- (coord-x c))  (- (coord-y c)) (- (coord-z c)))))
(define rotation-yN270  (lambda (c) (make-coord (- (coord-z c))  (- (coord-y c)) (+ (coord-x c)))))

(define rotation-xN0    (lambda (c) (make-coord (- (coord-x c))  (+ (coord-y c)) (+ (coord-z c)))))
(define rotation-xN90   (lambda (c) (make-coord (- (coord-x c))  (- (coord-z c)) (+ (coord-y c)))))
(define rotation-xN180  (lambda (c) (make-coord (- (coord-x c))  (- (coord-y c)) (- (coord-z c)))))
(define rotation-xN270  (lambda (c) (make-coord (- (coord-x c))  (+ (coord-z c)) (- (coord-y c)))))

;;(define all-rotations-func (list rotation-zP0 rotation-zP90 rotation-zP180 rotation-zP270
;;                                 rotation-yP0 rotation-yP90 rotation-yP180 rotation-yP270
;;                                 rotation-xP0 rotation-xP90 rotation-xP180 rotation-xP270
;;                                 rotation-zN0 rotation-zN90 rotation-zN180 rotation-zN270
;;                                 rotation-yN0 rotation-yN90 rotation-yN180 rotation-yN270
;;                                 rotation-xN0 rotation-xN90 rotation-xN180 rotation-xN270))

(define all-rotations-func (list
                            ;;FIXME: supprimer les redondances
                             (lambda (c) (make-coord (+ (coord-x c)) (+  (coord-y c))  (+ (coord-z c))))
                             (lambda (c) (make-coord (+ (coord-x c))  (+ (coord-y c))  (- (coord-z c))))
                             (lambda (c) (make-coord (+ (coord-x c))  (- (coord-y c))  (+ (coord-z c))))
                             (lambda (c) (make-coord (+ (coord-x c))  (- (coord-y c))  (- (coord-z c))))
                             (lambda (c) (make-coord (- (coord-x c))  (+ (coord-y c))  (+ (coord-z c))))
                             (lambda (c) (make-coord (- (coord-x c))  (+ (coord-y c))  (- (coord-z c))))
                             (lambda (c) (make-coord (- (coord-x c))  (- (coord-y c))  (+ (coord-z c))))
                             (lambda (c) (make-coord (- (coord-x c))  (- (coord-y c))  (- (coord-z c))))
                             (lambda (c) (make-coord (+ (coord-x c))  (+ (coord-z c))  (+ (coord-y c))))
                             (lambda (c) (make-coord (+ (coord-x c))  (+ (coord-z c))  (- (coord-y c))))
                             (lambda (c) (make-coord (+ (coord-x c))  (- (coord-z c))  (+ (coord-y c))))
                             (lambda (c) (make-coord (+ (coord-x c))  (- (coord-z c))  (- (coord-y c))))
                             (lambda (c) (make-coord (- (coord-x c))  (+ (coord-z c))  (+ (coord-y c))))
                             (lambda (c) (make-coord (- (coord-x c))  (+ (coord-z c))  (- (coord-y c))))
                             (lambda (c) (make-coord (- (coord-x c))  (- (coord-z c))  (+ (coord-y c))))
                             (lambda (c) (make-coord (- (coord-x c))  (- (coord-z c))  (- (coord-y c))))
                             (lambda (c) (make-coord (+ (coord-y c))  (+ (coord-x c))  (+ (coord-z c))))
                             (lambda (c) (make-coord (+ (coord-y c))  (+ (coord-x c))  (- (coord-z c))))
                             (lambda (c) (make-coord (+ (coord-y c))  (- (coord-x c))  (+ (coord-z c))))
                             (lambda (c) (make-coord (+ (coord-y c))  (- (coord-x c))  (- (coord-z c))))
                             (lambda (c) (make-coord (- (coord-y c))  (+ (coord-x c))  (+ (coord-z c))))
                             (lambda (c) (make-coord (- (coord-y c))  (+ (coord-x c))  (- (coord-z c))))
                             (lambda (c) (make-coord (- (coord-y c))  (- (coord-x c))  (+ (coord-z c))))
                             (lambda (c) (make-coord (- (coord-y c))  (- (coord-x c))  (- (coord-z c))))
                             (lambda (c) (make-coord (+ (coord-y c))  (+ (coord-z c))  (+ (coord-x c))))
                             (lambda (c) (make-coord (+ (coord-y c))  (+ (coord-z c))  (- (coord-x c))))
                             (lambda (c) (make-coord (+ (coord-y c))  (- (coord-z c))  (+ (coord-x c))))
                             (lambda (c) (make-coord (+ (coord-y c))  (- (coord-z c))  (- (coord-x c))))
                             (lambda (c) (make-coord (- (coord-y c))  (+ (coord-z c))  (+ (coord-x c))))
                             (lambda (c) (make-coord (- (coord-y c))  (+ (coord-z c))  (- (coord-x c))))
                             (lambda (c) (make-coord (- (coord-y c))  (- (coord-z c))  (+ (coord-x c))))
                             (lambda (c) (make-coord (- (coord-y c))  (- (coord-z c))  (- (coord-x c))))
                             (lambda (c) (make-coord (+ (coord-z c))  (+ (coord-x c))  (+ (coord-y c))))
                             (lambda (c) (make-coord (+ (coord-z c))  (+ (coord-x c))  (- (coord-y c))))
                             (lambda (c) (make-coord (+ (coord-z c))  (- (coord-x c))  (+ (coord-y c))))
                             (lambda (c) (make-coord (+ (coord-z c))  (- (coord-x c))  (- (coord-y c))))
                             (lambda (c) (make-coord (- (coord-z c))  (+ (coord-x c))  (+ (coord-y c))))
                             (lambda (c) (make-coord (- (coord-z c))  (+ (coord-x c))  (- (coord-y c))))
                             (lambda (c) (make-coord (- (coord-z c))  (- (coord-x c))  (+ (coord-y c))))
                             (lambda (c) (make-coord (- (coord-z c))  (- (coord-x c))  (- (coord-y c))))
                             (lambda (c) (make-coord (+ (coord-z c))  (+ (coord-y c))  (+ (coord-x c))))
                             (lambda (c) (make-coord (+ (coord-z c))  (+ (coord-y c))  (- (coord-x c))))
                             (lambda (c) (make-coord (+ (coord-z c))  (- (coord-y c))  (+ (coord-x c))))
                             (lambda (c) (make-coord (+ (coord-z c))  (- (coord-y c))  (- (coord-x c))))
                             (lambda (c) (make-coord (- (coord-z c))  (+ (coord-y c))  (+ (coord-x c))))
                             (lambda (c) (make-coord (- (coord-z c))  (+ (coord-y c))  (- (coord-x c))))
                             (lambda (c) (make-coord (- (coord-z c))  (- (coord-y c))  (+ (coord-x c))))
                             (lambda (c) (make-coord (- (coord-z c))  (- (coord-y c))  (- (coord-x c))))))

;;(for-each
;; (cut format #t "~a\n" <>)
;; (sort (map transform->string all-rotations-func) string<?))

(define all-rotations-sym '( P0 zP90 zP180 zP270
                                yP90 yP180 yP270
                                xP90 xP180 xP270
                            zN0 zN90  N180 zN270
                            yN0 yN90       yN270
                            xN0 xN90       xN270))

(define all-rotations
   (list
    (cons 'P0   rotation-zP0)
    (cons 'zP0   rotation-zP0)
    (cons 'zP90  rotation-zP90)
    (cons 'zP180 rotation-zP180)
    (cons 'zP270 rotation-zP270)
    (cons 'yP90  rotation-yP90)
    (cons 'yP180 rotation-yP180)
    (cons 'yP270 rotation-yP270)
    (cons 'xP90  rotation-xP90)
    (cons 'xP180 rotation-xP180)
    (cons 'xP270 rotation-xP270)
    (cons 'zN0   rotation-zN0)
    (cons 'zN90  rotation-zN90)
    (cons 'N180  rotation-zN180)
    (cons 'zN180  rotation-zN180)
    (cons 'zN270 rotation-zN270)
    (cons 'yN0   rotation-yN0)
    (cons 'yN90  rotation-yN90)
    (cons 'yN180 rotation-yN180)
    (cons 'yN270 rotation-yN270)
    (cons 'xN0   rotation-xN0)
    (cons 'xN90  rotation-xN90)
    (cons 'xN180 rotation-xN180)
    (cons 'xN270  rotation-xN270)))

(define-record-type <transform>
  (make-transform rotation translation)
  transform?
  (rotation transform-rotation)
  (translation transform-translation))

(define (transform-apply transform)
   (let* ((rotation (cdr (assoc (transform-rotation transform) all-rotations)))
          (translation (transform-translation transform)))
      (lambda (coord)
        (let* ((coord (rotation coord))
               (x (+ (coord-x coord) (coord-x translation)))
               (y (+ (coord-y coord) (coord-y translation)))
               (z (+ (coord-z coord) (coord-z translation))))
           (make-coord x y z)))))

(define (transform-for-scan scan1 scan0)
   (let* ((dstpairs1 (scan->pairs-by-distances scan1))
          (dstpairs0 (scan->pairs-by-distances scan0))
          (dst-init (get-first-pair-with-same-distance dstpairs1 dstpairs0))
          (pair1 (cdr (assoc dst-init dstpairs1))) (p1a (car pair1)) (p1b (cdr pair1))
          (pair0 (cdr (assoc dst-init dstpairs0))) (p0a (car pair0)) (p0b (cdr pair0))
          (transforms (append
                       (get-transforms-candidates p1a p0a)
                       (get-transforms-candidates p1a p0b))))
     (let loop ((dstpairs1 dstpairs1) (transforms transforms))
        (cond
         ((null? dstpairs1)
          (cond
           ((= 1 (length transforms)) (car transforms))
           ((null? transforms) (error "no transform left"))
           ((all-transform-equals? transforms) (car transforms))
           (else (error "still too many transforms"))))
         (else (let* ((cur (car dstpairs1))
                      (rest (cdr dstpairs1))
                      (dst (car cur))
                      (cur0 (assoc dst dstpairs0))
                      (transforms (if cur0
                                      (filter (cute is-possible-transform? (cdr cur0) (cdr cur) <>) transforms)
                                      transforms)))
                  (loop rest transforms)))))))

(define (scanners-as-pairs scanners)
  (map scan-beacons scanners))

(define (extract-distances scans)
  (let* (
         (x (map scan->pairs-by-distances scans))
         (x (map (lambda (scan) (map car scan))
                 x)))
    x))

(define (car-default-false l)
   (cond
    ((null? l) #f)
    ((list? l) (car l))
    (else #f)))

(define (zip-aligned . lst)
   (let zip-aligned-rec ((lst lst) (result '()))
     (cond
      ((fold (lambda (cur acc) (and acc (null? cur))) #t lst) (reverse result))
      (else
        (let* (
               (vals (map car-default-false lst))
               (cur (mymin coord<? (filter identity vals)))
               (rests (map (lambda (scan)
                               (cond ((equal? cur (car-default-false scan)) (cdr scan))
                                     (else scan)))
                           lst))
               (vals (map (lambda (x)
                              (cond ((equal? cur x) x)
                                    (else 'nil)))
                          vals)))
          (zip-aligned-rec rests (cons vals result)))))))

(define (zip-alists . lst)
    (let* ((keys (delete-duplicates (append-map (cut map car <>) lst))))
       (let zip-alists-rec ((keys keys) (result '()))
         (cond
          ((null? keys) (reverse result))
          (else
            (let* ((key (car keys))
                   (rest (cdr keys))
                   (cur (map (cut assoc key <>) lst))
                   (all? (every identity cur))
                   ;(_ (dbg "all?" all?))
                   (result (if all? (acons key (map cdr cur) result) result)))
              (zip-alists-rec rest result)))))))

(define (scans-print scans)
  (let* (
         (x (extract-distances scans))
         (x (apply zip-aligned x)))
     (for-each
       (lambda (c)
         (format #t "~a\n" (map coord->string c)))
       x)))


(define (scan-give-transform scan-from scan-to)
  ;; we give
  (let* ((pairs-from (scan-pairs scan-from))
         (pairs-to (scan-pairs scan-to))
         (zipped (zip-alists pairs-from pairs-to)))
      (let* ((first-distance-with-pairs (first zipped))
             (first-pairs (cdr first-distance-with-pairs))
             (first-pair-from (car first-pairs))
             (first-pair-to (cadr first-pairs))
             (result (fold
                      (lambda (cur-distance-with-pairs acc)
                        (let* ((pairs (cdr cur-distance-with-pairs))
                               (pair-from (first pairs))
                               (pair-to (second pairs))
                               (newacc (map
                                        (lambda (trwithcount)
                                            (let* ((tr (cdr trwithcount))
                                                   (validtr? (is-possible-transform? pair-to pair-from tr))
                                                   (newcount
                                                    (if validtr? (1+ (car trwithcount)) (car trwithcount)))
                                                   ;(_ (dbg "validtr?,nwecount=" (list validtr? newcount)))
                                                   ;(_ (usleep (quotient (expt 10 6) 3)))
                                                   ;(_ (if validtr? (dbg "acc=" (map car acc))))
                                                   (ret (cons newcount tr)))
                                               ret))
                                        acc)))

                         newacc))
                      (map
                       (cut cons 1 <>)
                       (append (get-transforms-candidates (car first-pair-from) (car first-pair-to))
                              (get-transforms-candidates (car first-pair-from) (cdr first-pair-to))))
                      zipped)))
           (sort result (lambda (a b) (> (car a) (car b)))))))

(map
 (lambda (x)
   (cons (1+ ( car x)) (cdr x)))
 '((0 . a)(1 . b)(5 . c)))
(define (scan-merge from to)
   (let* ((transform (scan-give-transform from to)))
          ;(_ (dbgn "trs=" (map transform->string transform))))
     (cond
       ((null? transform) #f)
       ((< (caar transform) 12) #f)
       (transform (let* ((transform (cdar transform))
                         (from-transformed (map transform (scan-beacons from)))
                         (result (append-reverse from-transformed (scan-beacons to))))
                    (make-scan (delete-duplicates result) (string-append (scan-name from) (scan-name to)))))
       (else #f))))

(define (merge-all-scanners lst)
   (let merge-rec ((rest (cdr lst)) (wait '()) (to (car lst)))
      (cond
       ((and (null? wait) (null? rest)) to)
       ((null? rest) (merge-rec (cons to (cdr wait)) '() (car wait)))
       (else (let* ((from (car rest))
                    (rest (cdr rest))
                    (_ (dbgn "rest,wait,to=" (list (map scan-name rest) "|" (map scan-name wait) "|" (scan-name to))))
                    (merged (scan-merge from to))
                    ;(_ (dbg "f,t,m=" (list (scan-length from) (scan-length to) (if merged (scan-length merged) #f))))
                    (_ (dbg "f,t,m=" (list (scan-name from) "|" (scan-name to) "|" (if merged (scan-name merged) #f)))))
                (cond
                 (merged (merge-rec rest wait merged))
                 (else (merge-rec rest (cons from wait) to))))))))

(define-public (main args)
   (let* ((scanners (read-all-scanners (current-input-port)))
          (fullmap (merge-all-scanners scanners))
          (result1 (scan-length fullmap))
          (result2 "UNIMP"))
     (format #t "result1: ~a\n" result1)
     (format #t "result2: ~a\n" result2)))


;; ;;;;;;;
;; 
;; 
;; ((0, 5, 31) (474, 580, 667) . (443, 580, 662)) ((0, 5, 31) (474, 580, 667) . (443, 580, 662))
;; 
;; (make-coord 474 580 667)
;; (make-coord 443 580 662)
;; 
;; 
;; 
;; 
;; (get-transforms-candidates $1 $1)
;; (map (cut apply <> (list $1)) $3)
;; (filter (cute is-possible-transform? (cons $1 $2) (cons $1 $2) <>) $3)


