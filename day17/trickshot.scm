(define-module (adventofcode2021 day17 trickshot)
    #:use-module (adventofcode2021 day17 utils)
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

;; Au départ on a:
;; x=0, y=0, vx=vx-init, vy=vy-init, tx-min, tx-max, ty-min, ty-max

(define-record-type <state>
    (make-state c v)
    state?
    (c state-c)
    (v state-v))

(define-record-type <velocities>
   (make-velocities vx vy)
   velocities?
   (vx velocities-vx)
   (vy velocities-vy))
(define (velocities<? a b)
  (or (< (velocities-vx a) (velocities-vx b))
      (and
        (= (velocities-vx a) (velocities-vx b))
        (< (velocities-vy a) (velocities-vy b)))))

(define-record-type <target>
    (make-target x-min x-max y-low y-high)
    target?
    (x-min target-x-min)
    (x-max target-x-max)
    (y-low target-y-low)
    (y-high target-y-high))
;; On suppose que l'aire cible est toujours en dessous du sous-marin
;; et donc y-low < y-high < 0

(define (string->coords str)
   (let* ((str (string-trim str))
          (start (string-index str #\=))
          (str (substring str (1+ start)))
          (separator (string-index str #\.))
          (first (string->number (substring str 0 separator)))
          (second (string->number (substring str (+ 2 separator)))))
      (cons first second)))

(define (string->target line)
  (let* ((start (string-length "target area: "))
         (coords (string-split (substring line start) #\,))
         (coordsx (string->coords (car coords)))
         (coordsy (string->coords (cadr coords))))
    (make-target (car coordsx) (cdr coordsx) (car coordsy) (cdr coordsy))))

;x=20..30, y=-10..-5

(define (vy->y-max vy-init)
   ;; y-max est atteint quand vy atteint 0
   ;; à ce moment y = somme de vy pour tous les vy de vy-init à 0
   (/ (* vy-init (1+ vy-init)) 2))

(define (vy-init-allmax target)
   ;; Quelque-soit vy-init>=0 la vitesse quand la sonde en redescente
   ;; atteint y=0 au est égale à - vy-init
   ;;
   ;; Donc on n'utilisera jamais vy-init<0 car on peut toujours trouver
   ;; un vx pour lequel la position x finale du sous-marin est dans
   ;; l'aire cible (on suppose qu'il y a un nombre triangulaire parmi
   ;; les x de l'aire cible) et donc avoir un vy-init positif.
   ;;
   ;; Si vy au moment du passage à x=0 est supérieur à ty-max on saute
   ;; par-dessus l'aire cible et donc on ne l'atteindra jamais. On peut
   ;; donc se restreindre à explorer les solutions de vy à vy-init-max.
   (1+ (- (target-y-low target))))

(define (vy-reach-target-at vy-init target)
 (let ((y-low (target-y-low target))
       (y-high (target-y-high target)))
   (let loop ((y 0) (vy vy-init) (t 0) (results '()))
      (cond
        ((< y y-low) results) ; we passed the target, we will not find more
        ((<= y y-high) ; between y-high and y-low, we are "in the target"
         (loop (+ y vy) (1- vy) (1+ t) (cons t results)))
        (else ; still above target, try next step
         (loop (+ y vy) (1- vy) (1+ t) results))))))

(define (vx-init-allmax target)
   (1+ (target-x-max target)))

(define (x-at vx-init t)
   (cond
    ((>= t vx-init) (sum-of-first-ns vx-init))
    (else (-
           (* t vx-init)
           (sum-of-first-ns (1- t))))))

(define (vx-in-target? vx-init t target)
   (let ((x-min (target-x-min target))
         (x-max (target-x-max target))
         (x (x-at vx-init t)))
     (and (<= x x-max) (>= x x-min))))

(define (any-vx-in-target? t target)
   (let ((x-min (target-x-min target))
         (x-max (target-x-max target)))
     (let loop ((vx-init (vx-init-allmax target)))
      (let ((x (x-at vx-init t)))
        (cond
         ((< x x-min) #f) ; we cannot reach target for this vx-init, will not be able for lower vx-init
         ((<= x x-max) #t) ; we are in the target
         (else (loop (1- vx-init)))))))) ; after the target, try lower vx

(define (all-vx-in-target-for-step t target)
   (let ((x-min (target-x-min target))
         (x-max (target-x-max target)))
     (let loop ((vx-init (vx-init-allmax target))
                (results '()))
      (let ((x (x-at vx-init t)))
        (cond
         ((< x x-min) results) ; we cannot reach target for this vx-init, will not be able for lower vx-init
         ((<= x x-max) (loop (1- vx-init) (cons vx-init results))) ; we are in the target
         (else (loop (1- vx-init) results))))))) ; after the target, try lower vx

(define (search-vy-init-max target)
   ;; y-max est atteint pour vy-max.
   ;; on parcours tous les vy-init posssibles en partant de vy-init-allmax
   ;; et à chaque on teste si on touche la cible en y.
   (let loop ((vy-init (vy-init-allmax target)))
     (let* ((yreach (vy-reach-target-at vy-init target))
            (xreach? (cond
                      ((null? yreach) #f)
                      (else (any-vx-in-target? (car yreach) target)))))
       (cond (xreach? vy-init) ; we found it
             ((< vy-init 0) #f) ; we will never find it
             (else (loop (1- vy-init))))))) ; we try again with a lower vy-init

(define (search-all-v-inits target)
   ;; on parcours tous les vy-init posssibles en partant de vy-init-allmax
   ;; et à chaque on teste si on touche la cible.
   (let loop ((vy-init (vy-init-allmax target))
              (results '()))
     (cond
        ((< vy-init (target-y-low target)) results) ; already under area, we will never find more
        (else
             (let* ((steps-reaching-target (vy-reach-target-at vy-init target))
                    (vx-inits (append-map
                                 (lambda (t) (all-vx-in-target-for-step t target))
                                 steps-reaching-target))
                    (vx-inits (delete-duplicates vx-inits))
                    (velocities (map (lambda (vx) (make-velocities vx vy-init)) vx-inits))
                    (results (append velocities results)))
                 (loop (1- vy-init) results)))))) ; we search more with a lower vy-init

(define (sum-of-first-ns n)
    (/ (* n (1+ n)) 2))

(define (search-y-max target)
  (let ((vy-init-max (search-vy-init-max target)))
    (cond
      (vy-init-max (sum-of-first-ns vy-init-max))
      (else #f))))

(define-public (main args)
   (let* ((line (read-line))
          (target (string->target line))
          (result1 (search-y-max target))
          (result2 (length (search-all-v-inits target))))
     (format #t "result1: ~a\n" result1)
     (format #t "result2: ~a\n" result2)))
