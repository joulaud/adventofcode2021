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

(define-record-type <coord>
    (make-coord line col)
    coord?
    (line coord-line)
    (col coord-col))
(define (coord->pair coord)
   (cons (coord-line coord) (coord-col coord)))
(define (pair->coord pair)
   (make-coord (car pair) (cdr pair)))
(define (array->gridsize grid)
  (let ((dims (array-dimensions grid)))
   (make-coord (car dims) (cadr dims))))

(define (neighbours loc gridsize)
  ;; I don't like it but it works
  (let* ((l (coord-line loc))
         (c (coord-col loc))
         (maxl (coord-line gridsize))
         (maxc (coord-col gridsize)))
      (filter
         identity
         (append-map
           (lambda (ld)
              (let ((lres (- l ld)))
                (if (and (>= lres 0) (< lres maxl))
                    (map (lambda (cd)
                            (let ((cres (- c cd)))
                               (if (and
                                     (>= cres 0) (< cres maxc) ; not out-of-grid
                                     (not (and (= 0 ld) (= 0 cd)))) ; not current loc
                                   (make-coord lres cres)
                                   #f)))
                       '(-1 0 1))
                    '())))
           '(-1 0 1)))))

;; (test-equal
;;    '((4 . 4) (4 . 3) (4 . 2) (3 . 4) (3 . 2) (2 . 4) (2 . 3) (2 . 2))
;;    (neighbours (make-coord 3 3) (make-coord 6 6)))

(define-record-type <grid>
    (make-grid array flashnum)
    grid?
    (array grid-array)
    (flashnum grid-flashnum grid-flashnum-set!))
(define (grid-flasnum-inc! grid)
    (grid-flashnum-set! grid (1+ (grid-flashnum grid))))

(define-record-type <octopus>
    (make-octopus energy flashing)
    octopus?
    (energy octopus-energy)
    (flashing octopus-flashing))
(define (new-octopus)
   (make-octopus 1 'notflashed))
(define (octopus-1+ octopus)
    (let* ((energy (octopus-energy octopus))
           (new-energy (1+ energy)))
      (make-octopus new-energy 'notflashed)))

(define (array-refloc grid loc)
   (array-ref grid (coord-line loc) (coord-col loc)))
(define (array-setloc! grid val loc)
   (array-set! grid val (coord-line loc) (coord-col loc)))

(define (initial-octopus-grid-increase grid)
 (let ((grid (grid-array grid)))
   (array-for-each-index
          grid
          (lambda (i j v) (array-set! grid (octopus-1+ v) i j))
          (lambda (i) #t))))

(define (print-octopus-grid grid)
 (let ((a (grid-array grid)))
  (array-for-each-index
       a
       (lambda (i j v)
           (let* ((energy (octopus-energy v))
                  (disp (if (> energy 9) "X" energy)))
             (display disp)))
       (lambda (i) (display "\n")))))

(define (octopus-flash-in-grid grid loc firstincrease)
   (let* ((grid-as-grid grid)
          (grid (grid-array grid))
          (octopus (array-refloc grid loc))
          (energy (octopus-energy octopus))
          (energy (if firstincrease (1+ energy) energy))
          (flashing (octopus-flashing octopus))
          (mustflash (and
                        (eqv? flashing 'notflashed)
                        (> energy 9))))
     (cond
       (mustflash
         (let ((neighbours-loc (neighbours loc (array->gridsize grid))))
            (array-setloc! grid (make-octopus 0 'flashed) loc)
            (grid-flasnum-inc! grid-as-grid)
            (for-each
              (lambda (loc) (octopus-flash-in-grid grid-as-grid loc #t))
              neighbours-loc)))
       ((eqv? flashing 'notflashed
            (array-setloc! grid (make-octopus energy 'notflashed) loc))))))


(define (octopus-flash-all grid)
   (array-for-each-index
       (grid-array grid)
       (lambda (l c o) (octopus-flash-in-grid grid (make-coord l c) #f))
       (lambda (l) #t)))

(define (iterate-flashes grid n)
  (fold
    (lambda (i acc)
        (initial-octopus-grid-increase grid)
        (octopus-flash-all grid))
    0
    (iota 100)))

(define (iterate-until-all-flashes grid)
  (let loop ((count 1))
        (grid-flashnum-set! grid 0)
        (initial-octopus-grid-increase grid)
        (octopus-flash-all grid)
        (if (>= (grid-flashnum grid) 100)
            count
            (loop (1+ count)))))

(define example-grid
 #2(
     ( 1 1 1 1 1)
     ( 1 9 9 9 1)
     ( 1 9 1 9 1)
     ( 1 9 9 9 1)
     ( 1 1 1 1 1)))
(define example-big
  #2(( 5 4 8 3 1 4 3 2 2 3)
     ( 2 7 4 5 8 5 4 7 1 1)
     ( 5 2 6 4 5 5 6 1 7 3)
     ( 6 1 4 1 3 3 6 1 4 6)
     ( 6 3 5 7 3 8 5 4 7 8)
     ( 4 1 6 7 5 2 4 6 4 5)
     ( 2 1 7 6 8 4 1 7 2 1)
     ( 6 8 8 2 8 8 1 1 3 4)
     ( 4 8 4 6 8 4 8 5 5 4)
     ( 5 2 8 3 7 5 1 5 2 6)))
(define (raw->octopus-grid raw)
  (let* ((dims (array-dimensions raw))
         (maxl (car dims))
         (maxc (cadr dims))
         (out (make-array (new-octopus) maxl maxc)))
    (array-for-each-index
       raw
       (lambda (l c n) (array-set! out (make-octopus n 'notflashed) l c))
       (lambda (l) #t))
    (make-grid out 0)))
     
;;(define ex1 (raw->octopus-grid! example-grid))
;;(define ex2 (raw->octopus-grid! example-big))
;;(print-octopus-grid ex2)
;;(iterate-flashes ex2 100)
;;(print-octopus-grid ex2)
;;(display (grid-flashnum ex2))


(define-public (main args)
  (let* (
         (strm (stream-of-lines (current-input-port)))
         (a (stream-of-lines->array strm))
         (grid (raw->octopus-grid a))
         (nul (iterate-flashes grid 100))
         (result1 (grid-flashnum grid))
         (grid (raw->octopus-grid a))
         (result2 (iterate-until-all-flashes grid)))
    (format #t "result1: ~a\n" result1)
    (format #t "result2: ~a\n" result2)))
