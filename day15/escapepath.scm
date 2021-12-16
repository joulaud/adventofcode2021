(define-module (adventofcode2021 day15 escapepath)
    #:use-module (adventofcode2021 day15 utils)
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

(define-record-type <coord>
    (make-coord line col)
    coord?
    (line coord-line)
    (col coord-col))
(define (coord->pair coord)
   (cons (coord-line coord) (coord-col coord)))
(define (pair->coord pair)
   (make-coord (car pair) (cdr pair)))
(define (array->maxcoord grid)
  (let ((dims (array-dimensions grid)))
   (make-coord (1- (car dims)) (1- (cadr dims)))))

(define (print-visited visited maxcoord)
  (let* ((lmax (coord-line maxcoord))
         (cmax (coord-col  maxcoord)))
   (let loop ((l 0))
      (if (<= l lmax)
          (begin
           (let loopc ((c 0))
            (if (<= c cmax)
                (let* ((val (assoc (make-coord l c) visited))
                       (val (and val (cdr val)))
                       (val (if val "#" ".")))
                  (display val)
                  (loopc (1+ c)))))
           (display "\n")
           (loop (1+ l)))))))

(define (read-cavemap port)
  (let* ((x (stream-of-lines port))
         (x (stream-of-lines->array line->numlist x)))
    x))

(define cavemap-ref array-ref)
(define (cavemap-refloc cavemap loc)
     (array-ref cavemap (coord-line loc) (coord-col loc)))
(define (cavemap-setloc! cavemap val loc)
     (array-set! cavemap val (coord-line loc) (coord-col loc))
     cavemap)


(define (ingrid? maxcoord)
 (lambda (loc)
  (let* (
         (l (coord-line loc))
         (c (coord-col loc))
         (lmax (coord-line maxcoord))
         (cmax (coord-col maxcoord)))
    (and (>= l 0)
         (>= c 0)
         (<= l lmax)
         (<= c cmax)))))

(define (neighbours loc gridsize)
  (let* ((up    '(-1 . 0))
         (down  '(1 . 0))
         (left  '(0 . -1))
         (right '(0 . 1))
         (l (coord-line loc))
         (c (coord-col loc))
         (neighbours (map
                       (lambda (direction)
                           (make-coord (+ l (car direction))
                                       (+ c (cdr direction))))
                       (list down right up left)))
         (neighbours (filter
                      (ingrid? gridsize)
                      neighbours)))
     neighbours))

(define (ormin a b)
    (cond
     ((and a b) (min a b))
     (a a)
     (b b)
     (else #f)))

(define (lowest-risk-internal cavemap visited loc initialrisk minpathfound depth)
   (dbg "minpathfound,depth=" (list minpathfound depth))
   ;;(dbg "F()minpathfound=" minpathfound)
   (let* ((endloc (array->maxcoord cavemap))
          (depth (1+ depth))
          (currisk (cavemap-refloc cavemap loc))
          (updatedrisk (+ currisk initialrisk))
          ;;(_ (dbg "loc,cur,risk,minpathfound=" (list loc currisk updatedrisk minpathfound)))
          (curvisited (assoc loc visited))
          (curvisited? (and curvisited (cdr curvisited)))
          (end? (equal? loc endloc))
          (updatedvisited (acons loc #t visited))
          (neighbours (neighbours loc endloc)))
     ;; (print-visited updatedvisited endloc)
     ;; (display "\n")
     (cond
      (curvisited? #f)
      (end? updatedrisk)
      ((and minpathfound (>= updatedrisk minpathfound)) minpathfound)
      (else
       (let* ((minrisk
               (fold
                (lambda (x minpathfound)
                    ;;(dbg "minpathfound" minpathfound)
                    (let* ((x (lowest-risk-internal cavemap updatedvisited x updatedrisk minpathfound depth))
                           (x (ormin minpathfound x)))
                      ;;(dbg "x=" x)
                      x))
                minpathfound
                neighbours)))
           minrisk)))))

(define (inf<=? a b)
    (cond
     ((eqv? a 'inf)
      (if (eqv? b 'inf) #t #f))
     ((eqv? b 'inf) #t)
     (else (<= a b))))


(define (minimum-in-array a stillhere stillherelastcols)
  (let* ((dims (array-dimensions a))
         (maxl (car dims))
         (maxc (cadr dims))
         (lres #f)
         (cres #f)
         (valmin 'inf))
    (do ((i 0 (1+ i)))
        ((>= i maxl))
     (let* ((lastcol (array-ref stillherelastcols i))
            (futurelastcol lastcol))
      (do ((j 0 (1+ j)))
          ((or (>= j maxc) (> j lastcol)))
         (cond ((array-ref stillhere i j)
                (set! futurelastcol j)
                (let* ((val (array-ref a i j))
                       (ismin? (inf<=? val valmin)))
                    (cond (ismin?
                           (set! lres i)
                           (set! cres j)
                           (set! valmin val)))))))
      (array-set! stillherelastcols futurelastcol i)))
    (cond
      (lres (cons stillherelastcols (make-coord lres cres)))
      (else (cons #f #f)))))

(define (minimum-in-alist to-be-evaluated)
  (let loop ((to-be-evaluated to-be-evaluated) (elem-min #f) (still-to-evaluate '()))
      (cond
       ((null? to-be-evaluated) (cons elem-min still-to-evaluate))
       (else (let* ((elem-cur (car to-be-evaluated))
                    (rest (cdr to-be-evaluated)))
                 (cond
                   (elem-min
                    (let* ((min-cur? (< (cdr elem-cur) (cdr elem-min)))
                           (still-to-evaluate (cons (if min-cur? elem-min elem-cur) still-to-evaluate))
                           (elem-min (if min-cur? elem-cur elem-min)))
                     (loop rest elem-min still-to-evaluate)))
                   (else (loop rest elem-cur still-to-evaluate))))))))

(define (compute-distances cavemap)
   (let* ((endloc (array->maxcoord cavemap))
          (dist (make-array 'inf (1+ (coord-line endloc)) (1+ (coord-col endloc))))
          (_ (array-set! dist 0 (coord-line endloc) (coord-col endloc)))
          (stillhere (make-array #t (1+ (coord-line endloc)) (1+ (coord-col endloc))))
          (stillherelastcols (make-array (1+ (coord-col endloc)) (1+ (coord-line endloc))))
          (to-be-evaluated (acons endloc 0 '())))
     (compute-distances-internal cavemap dist stillhere endloc
                                 0 (* (1+ (coord-line endloc)) (1+ (coord-col endloc)))
                                 stillherelastcols
                                 to-be-evaluated)))

(define (howmany stillhere)
   (let ((count 0))
     (array-for-each
       (lambda (x)
           (if x (set! count (1+ count))))
       stillhere)
     count))

(define (compute-distances-internal cavemap distances stillhere endloc count stillherecount stillherelastcols to-be-evaluated)
 ;; Here we use Dijkstra's algorithm as I understood it by a cursory glance at
 ;; http://algowiki-project.org/en/Dijkstra's_algorithm#Computational_kernel_of_the_algorithm
 ;; https://www.boost.org/doc/libs/1_78_0/libs/graph/doc/dijkstra_shortest_paths.html
 ;; It does not uses a real queue but only a marker (in STILLHERE)
 ;; and at each step must scan all map to find the current nearest point to the end.

 ;; - CAVEMAP is the problem description, we never change it
 ;; - DISTANCES is the result we are building with distances to END
 ;; - ENDLOC is the position of END
 ;; - COUNT and STILLHERECOUNT are just debugging counters to display
 ;;   some progression info while running
 ;; - STILLHERE tracks which node were visited and which node were not
 ;; - STILLHERELASTCOLS is an optimisation table to avoid scanning all
 ;;   cells in STILLHERE
 ;; - TO-BE-EVALUATED, is the queue to track cells we want to visit
 (let* ((x               (minimum-in-alist to-be-evaluated))
        (cur-elem        (car x))
        (to-be-evaluated (cdr x))
        (cur-distance    (cdr cur-elem))
        (cur             (car cur-elem)))
   (cond
    ((equal? cur (make-coord 0 0)) distances) ;; early exit, we found the minimum from start point
    (else
      ;(display count)
      (if (= 0 (remainder count 100))
          (begin
           (dbg "stillherecount=" stillherecount)))
      (let ((_ (array-set! stillhere #f (coord-line cur) (coord-col cur)))
            (stillherecount (1- stillherecount))
            (neighbours (filter (lambda (x) (cavemap-refloc stillhere x)) (neighbours cur endloc))))
        (let loop ((neighbours neighbours)
                   (distances distances)
                   (to-be-evaluated to-be-evaluated))
             (if (null? neighbours)
                 (compute-distances-internal cavemap distances stillhere endloc (1+ count) stillherecount stillherelastcols to-be-evaluated)
                 (let* ((curneighbour (car neighbours))
                        (rest (cdr neighbours))
                        (proposed-distance (+ (cavemap-refloc distances cur) (cavemap-refloc cavemap cur)))
                        (previous-distance (cavemap-refloc distances curneighbour))
                        (better? (inf<=? proposed-distance previous-distance))
                        (distances (if better?
                                       (cavemap-setloc! distances proposed-distance curneighbour)
                                       distances))
                        (to-be-evaluated (cond
                                          ((and better? (cavemap-refloc stillhere curneighbour))
                                           (assoc-set! to-be-evaluated curneighbour proposed-distance))
                                          (else to-be-evaluated))))
                    (loop rest distances to-be-evaluated)))))))))

(define (lowest-risk cavemap)
  (let* ((distances (compute-distances cavemap))
         (lowestrisk (array-ref distances 0 0)))
    lowestrisk))

(define (lowest-risk-old cavemap)
  (let* ((start-loc (make-coord 0 0))
         (start-val (cavemap-refloc cavemap start-loc))
         (lowestrisk (lowest-risk-internal cavemap '() start-loc (- start-val) #f 0)))
    lowestrisk))

(define (modulo9 x)
 (let* ((minus1 (1- x))
        (rem (remainder minus1 9))
        (res (1+ rem)))
    res))

(define (bigger-cavemap cavemap)
  (let* ((dims (array-dimensions cavemap))
         (lsize (car dims))
         (csize (cadr dims))
         (bigger (make-array #f (* 5 lsize) (* 5 csize))))
    (array-for-each-only-index
      bigger
      (lambda (l c)
       (begin
        (let* ((verticaltile (quotient l lsize))
               (horiztile (quotient c csize))
               (ratio (+ verticaltile horiztile))
               (val (array-ref cavemap (remainder l lsize) (remainder c csize)))
               (val (+ ratio val))
               (val (modulo9 val)))
         (array-set! bigger val l c)))))
    bigger))

(define-public (main args)
  (let* ((cavemap (read-cavemap (current-input-port)))
         (result1 (lowest-risk cavemap))
         (bigger (bigger-cavemap cavemap))
         (result2 (lowest-risk bigger)))
     (format #t "result1: ~a\n" result1)
     (format #t "result2: ~a\n" result2)))


