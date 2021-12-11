(define-module (first)
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
(use-modules (srfi srfi-1)) ; lists library
(use-modules (srfi srfi-11)) ; let*-values
(use-modules (srfi srfi-9))
(use-modules (srfi srfi-9 gnu)) ; immutable records
(use-modules (srfi srfi-41))

(define (dbg t v) (format #t "~s: ~a\n" t v) (force-output))

(define stream-of-lines
  (stream-lambda (port)
     (let
         ((line (read-line port)))
      (cond
       ((eof-object? line) stream-null)
       (else (stream-cons line (stream-of-lines port)))))))

(define (parse-line line)
   (let* (
          (x (string->list line))
          (x (map char->integer x))
          (x (map (lambda (v) (- v (char->integer #\0))) x)))
     x))

(define (stream-of-lines->array strm)
   (let* ((x (stream-map parse-line strm))
          (x (stream->list x))
          (x (list->array 2 x)))
     x))

(define (neighbours l c maxl maxc)
  ;; FIXME: this give too many neighbours as the text states explicitly
  ;; "Diagonal locations do not count as adjacent"
  ;; and "locations have four adjacent locations (up, down, left, and right)"
  (filter
     (lambda (x) (begin
                  (and x (not (equal? x (cons l c))))))
     (append-map
       (lambda (ld)
          (let ((lres (- l ld)))
            (if (and (>= lres 0) (< lres maxl))
                (map (lambda (cd)
                        (let ((cres (- c cd)))
                           (if (and (>= cres 0) (< cres maxc))
                               (cons lres cres)
                               #f)))
                     '(-1 0 1))
                '())))
       '(-1 0 1))))


;;; (define (lower a l c)
;;;    (fold
;;;       (lambda (cur acc)
;;;          (let ((curval (array-ref a (car cur) (cdr cur))))
;;;            (if (not acc)
;;;                curval
;;;                (min curval acc))))
;;;     #f
;;;     (let* ((dims (array-dimensions a))
;;;            (maxl (car dims))
;;;            (maxc (cadr dims)))
;;;        (dbg "l" l)
;;;        (dbg "c" c)
;;;        (dbg "maxl" maxl)
;;;        (dbg "maxc" maxc)
;;;        (neighbours l c maxl maxc))))

(define (low? a l c)
  (let* ((dims (array-dimensions a))
         (maxl (car dims))
         (maxc (cadr dims))
         (thisval (array-ref a l c)))
     (let loop ((neighbours (neighbours l c maxl maxc)))
            (if (eqv? '() neighbours)
                #t
                (let* ((curpos (car neighbours))
                       (curval (array-ref a (car curpos) (cdr curpos))))
                   (if (<= curval thisval)
                       #f
                       (loop (cdr neighbours))))))))

(define (array-for-each-index a proc-cell proc-endofline)
  (let* ((dims (array-dimensions a))
         (maxl (car dims))
         (maxc (cadr dims)))
    (do ((i 0 (1+ i)))
        ((>= i maxl))
      (do ((j 0 (1+ j)))
          ((>= j maxc))
        (proc-cell i j (array-ref a i j)))
      (proc-endofline i))))

(define (print-array a)
  (array-for-each-index
       a
       (lambda (i j v) (if v (display "X") (display ".")))
       (lambda (i) (display "\n"))))

(define (all-lows a)
  (let* ((dims (array-dimensions a))
         (maxl (car dims))
         (maxc (cadr dims))
         (out (make-array 'u maxl maxc)))
    (array-for-each-index
         a
         (lambda (i j v) (array-set! out (if (low? a i j) 'x 'n) i j))
         (lambda (i) #f))
    out))

(define (risk-level a)
  (let* ((dims (array-dimensions a))
         (maxl (car dims))
         (maxc (cadr dims))
         (out 0))
    (array-for-each-index
         a
         (lambda (i j v) (set! out (if (low? a i j) (+ out 1 v) out)))
         (lambda (i) #f))
    out))


(define (adjacent-locations pos numl numc)
   ;; 0,0 is the top-left corner
   (let* ((l (car pos))
          (c (cdr pos))
          (upl (1- l))
          (up (if (>= upl 0) (cons upl c) #f))
          (downl (1+ l))
          (down (if (< downl numl) (cons downl c) #f))
          (leftc (1- c))
          (left (if (>= leftc 0) (cons l leftc) #f))
          (rightc (1+ c))
          (right (if (< rightc numc) (cons l rightc) #f))
          (locations (filter identity (list up down left right))))
     locations))

(define (on-basin? heightmap basinmap pos)
  (let* ((dims (array-dimensions heightmap))
         (numlines (car dims))
         (numcols (cadr dims))
         (cur-height (array-ref heightmap (car pos) (cdr pos))))
    (if (= 9 cur-height)
        #f ; Locations of height 9 do not count as being in any basin
        (let loop ((locations (adjacent-locations pos numlines numcols)))
           (if (eqv? '() locations)
               #f
               (let ((res (and-let* (
                                     (adjloc (car locations))
                                     (adjl (car adjloc))
                                     (adjc (cdr adjloc))
                                     (adjacent-on-basin
                                       (array-ref basinmap adjl adjc))
                                     (adj-height
                                       (array-ref heightmap adjl adjc))
                                     (higher-than-neighbour (>= cur-height adj-height))))))
                  (if res
                      #t
                      (loop (cdr locations)))))))))

(define (iteration-on-basin heightmap basinmap)
  (let ((count 0))
    (array-for-each-index
       basinmap
       (lambda (l c v)
           (if (begin
                 v)
               #f ; already counted on basin
               (if (on-basin? heightmap basinmap (cons l c))
                   (begin (array-set! basinmap #t l c)
                          (set! count (1+ count))))))
       (lambda (l) #f))
    count))


(define (basin-size heightmap lowpoint)
  (let* ((dims (array-dimensions heightmap))
         (maxl (car dims))
         (maxc (cadr dims))
         (basinmap (make-array #f maxl maxc))
         (nul (array-set! basinmap #t (car lowpoint) (cdr lowpoint))))
    (let loop ((total 1)) ; we begin with one lowpoint on basin
        (let ((cur (iteration-on-basin heightmap basinmap)))
          (if (= cur 0)
              total
              (loop (+ total cur)))))))

(define (basin-sizes a)
  (let* ((dims (array-dimensions a))
         (maxl (car dims))
         (maxc (cadr dims))
         (out '()))
    (array-for-each-index
         a
         (lambda (i j v) (set! out (if (low? a i j) (cons (basin-size a (cons i j)) out) out)))
         (lambda (i) #f))
    out))

(define (max3 l)
   (let ((sorted (sort l >=)))
     (list-head sorted 3)))

(define (result2 heightmap)
   (let* ((sizes (basin-sizes heightmap))
          (three-big (max3 sizes)))
    (apply * three-big)))


(define-public (main args)
  (let* (
         (strm (stream-of-lines (current-input-port)))
         (a (stream-of-lines->array strm))
         (result1 (risk-level a))
         (result2 (result2 a)))
    (format #t "result1: ~a\n" result1)
    (format #t "result2: ~a\n" result2)))
