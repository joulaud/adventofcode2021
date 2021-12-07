(define-module (first)
    #:use-module (ice-9 rdelim)
    #:use-module (ice-9 match)
    #:use-module (ice-9 format)
    #:use-module (srfi srfi-1) ; lists library
    #:use-module (srfi srfi-9)
    #:use-module (srfi srfi-9 gnu) ; immutable records
    #:use-module (srfi srfi-41))

(define-record-type <bingo-grid>
  (make-bingo-grid-internal numbers lines-count cols-count)
  bingo-grid?
  (numbers bingo-numbers)
  (lines-count bingo-lines-count)
  (cols-count bingo-cols-count))

(define (number-list->positions lst)
    ;; LST is a list of list of numbers
    ;; This function returns a hash-map where keys are the numbers and
    ;; values their coordinates in the grid
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
                   (hash-set! h (car line) (cons lineno linecol))
                   (loopcol (cdr line) (+ linecol 1)))))
            (loopline (cdr lst) (+ lineno 1)))))
    h)

(define (make-bingo-grid number-list)
  ;; NUMBER-LIST should be a list of list of numbers like '((1 2 3) (4 5 6))
  ;; we assume each internal list have the same length as it represents a rectangle grid
  (let* ((lines (length number-list))
         (cols (length (car number-list)))
         (positions (number-list->positions number-list))
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
             ((eof-object? line) result)
             ((string= "" line) result)
             (else (loop (cons (line->numlist line) result)))))))

(define (bingo-tick grid number)
   ;; Note: we assume one number is only announced once
   ;; if same number is announced several times we might return #t as
   ;; winning position even if it is not
   (let* (
          (positions (bingo-numbers grid))
          (pos (hash-ref positions number))
          (line (car pos))
          (col (cdr pos))
          (line-ticks (bingo-lines-count grid))
          (line-tick (+ 1 (vector-ref line-ticks line)))
          (col-size (vector-length line-ticks))
          (col-ticks (bingo-cols-count grid))
          (col-tick (+ 1 (vector-ref col-ticks col)))
          (line-size (vector-length col-ticks)))
      (vector-set! line-ticks line line-tick)
      (vector-set! col-ticks col col-tick)
      (cond
         ((>= line-tick line-size) #t)
         ((>= col-tick col-size) #t)
         (else #f))))


(define-public (main args)
   (format #t "UNIMP\n"))
