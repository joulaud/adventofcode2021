(define-module (adventofcode secondwithstreams)
    #:use-module (ice-9 rdelim)
    #:use-module (srfi srfi-41))

(define stream-of-lines
  (stream-lambda ()
                 (let
                     ((line (read-line)))
                  (cond
                   ((eof-object? line) stream-null)
                   (else (stream-cons line (stream-of-lines)))))))

(define (lines->nums lines)
  (stream-filter identity ;; filter out #f due to strings not recognised as numbers
    (stream-map string->number lines)))

(define (sliding-windows-sums nums)
     (stream-map +
                 nums
                 (stream-cdr nums)
                 (stream-cdr (stream-cdr nums))))

(define (is-increment nums)
    (stream-map >
                (stream-cdr nums)
                nums))

(define (count-true strm)
    (stream-fold
     (lambda (acc cur)
         (if cur (+ 1 acc) acc))
     0
     strm))

(define (count-increments-of-sliding-windows)
    (let* ((x (stream-of-lines))
           (x (lines->nums x))
           (x (sliding-windows-sums x))
           (x (is-increment x))
           (x (count-true x)))
        x))
;; Is the more legible than this single line ?
;;    (count-true (is-increment (windowed-sums (lines->nums (stream-of-lines))))))


(display (count-increments-of-sliding-windows))
