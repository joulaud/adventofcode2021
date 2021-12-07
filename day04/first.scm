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
    ;; we suppose a 5x5 grid so we initialize hash table to 25
    (define h (make-hash-table 25))
    (let loopline ((lst lst) (lineno 1))
      (if (eqv? '() lst) #t
          (begin
            (let loopcol ((line (car lst)) (linecol 1))
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

(define-public (main args)
   (format #t "UNIMP\n"))
