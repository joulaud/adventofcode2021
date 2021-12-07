(define-module (first)
    #:use-module (ice-9 rdelim)
    #:use-module (ice-9 match)
    #:use-module (ice-9 format)
    #:use-module (srfi srfi-1) ; lists library
    #:use-module (srfi srfi-9)
    #:use-module (srfi srfi-9 gnu) ; immutable records
    #:use-module (srfi srfi-41))

(define-record-type <bingo-grid>
  (make-bingo-grid-internal cells lines-count cols-count)
  bingo-grid?
  (cells bingo-cells)
  (lines-count bingo-lines-count)
  (cols-count bingo-cols-count))

(define (number-list->cells lst)
    ;; LST is a list of list of numbers
    ;; This function returns a hash-map where keys are the numbers and
    ;; values are cells representing each number in the grid.
    ;; Each cell is '((x . y) . tick) where X and X are coordinates and
    ;; TICK allow to know if number was ticked or not.
    ;; We assume one number is only present once in the grid, else only
    ;; the last coordinates are keeped.
    ;; We suppose a 5x5 grid by default so we initialize hash table to 25
    (define h (make-hash-table 25))
    (let loopline ((lst lst) (lineno 0))
      (if (eqv? '() lst) #t
          (begin
            (let loopcol ((line (car lst)) (linecol 0))
               (if (eqv? '() line) #t
                 (begin
                   (hash-set! h (car line) (cons (cons lineno linecol) #f))
                   (loopcol (cdr line) (+ linecol 1)))))
            (loopline (cdr lst) (+ lineno 1)))))
    h)

(define (make-bingo-grid number-list)
  ;; NUMBER-LIST should be a list of list of numbers like '((1 2 3) (4 5 6))
  ;; we assume each internal list have the same length as it represents a rectangle grid
  (let* ((lines (length number-list))
         (cols (length (car number-list)))
         (positions (number-list->cells number-list))
         (lines-count (make-vector lines 0))
         (cols-count (make-vector cols 0))
         (bingo-grid (make-bingo-grid-internal positions lines-count cols-count)))
    bingo-grid))

(define (line->numlist line)
  (let* ((x line)
         (x (string-split x #\space))
         (x (filter
              (lambda (s) (not (string= "" s)))
              x))
         (x (map string->number x)))
    x))

(define (read-grid->numlistlist)
    ;; FIXME: due to the way we construct this list, line order is reversed (not a problem for now)
    (let loop ((result '()))
         (let ((line (read-line)))
           (cond
             ((eof-object? line) (cons line result))
             ((string= "" line) (cons line result))
             (else (loop (cons (line->numlist line) result)))))))

(define (bingo-tick grid number)
   ;; Note: we assume one number is only announced once
   ;; if same number is announced several times we might return #t as
   ;; winning position even if it is not
   (let* (
          (cells (bingo-cells grid))
          (cell (hash-ref cells number))
          (pos (car cell))
          (line (car pos))
          (col (cdr pos))
          (line-ticks (bingo-lines-count grid))
          (line-tick (+ 1 (vector-ref line-ticks line)))
          (col-size (vector-length line-ticks))
          (col-ticks (bingo-cols-count grid))
          (col-tick (+ 1 (vector-ref col-ticks col)))
          (line-size (vector-length col-ticks)))
      ;; track number of cells ticked in lines and cols
      (vector-set! line-ticks line line-tick)
      (vector-set! col-ticks col col-tick)
      ;; update tick mark for current number
      (hash-set! cells number (cons pos #t))
      ;; return true if grid is now winning
      (cond
         ((>= line-tick line-size) #t)
         ((>= col-tick col-size) #t)
         (else #f))))

(define (bingo-score grid)
  (let ((cells (bingo-cells grid)))
    (hash-fold
      (lambda (number cell acc)
           (if (not (cdr cell))
               (+ number acc)
               acc))
      0
      cells)))

(define (bingo-grids)
  (let loop ((result '()))
      (let* ((res (read-grid->numlistlist))
             (numlist (cdr res))
             (eof (eof-object? (car res)))
             (result (cons (make-bingo-grid numlist) result)))
        (if eof result
            (loop result)))))

(define (play-one-grid grid number)
   (let ((win? (bingo-tick grid number)))
     (if win? (bingo-score grid)
         #f)))

(define (play-all-grids grids number)
    (let*  ((x
              (map
                (lambda (grid)
                   (play-one-grid grid number))
               grids))
            (x (filter identity x)))
     (if (eq? '() x) #f
         (car x))))

(define (play-bingo)
  (let* ((drawed-numbers-line (read-line))
         (drawed-numbers-list (string-split drawed-numbers-line #\,))
         (drawed-numbers-list (map string->number drawed-numbers-list))
         (empty-line (read-line))
         (grids (bingo-grids)))
   (let loop ((drawed-numbers-list drawed-numbers-list))
       (let ((res (play-all-grids grids (car drawed-numbers-list))))
          (if res (* (car drawed-numbers-list) res)
              (loop (cdr drawed-numbers-list)))))))




(define-public (main args)
   (play-bingo))
