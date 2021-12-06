(define-module (second)
    #:use-module (ice-9 rdelim)
    #:use-module (ice-9 match)
    #:use-module (ice-9 format)
    #:use-module (srfi srfi-1) ; lists library
    #:use-module (srfi srfi-9)
    #:use-module (srfi srfi-9 gnu) ; immutable records
    #:use-module (srfi srfi-41))

(define (dbg t v) (format #t "~s: ~a\n" t v))

(define stream-of-lines
  (stream-lambda ()
     (let
         ((line (read-line)))
      (cond
       ((eof-object? line) stream-null)
       (else (stream-cons line (stream-of-lines)))))))

(define (bit-criteria-filter criteria lst)
   (define (rec-bit-criteria-filter lst pos)
       (cond
        ((<= (length lst) 1) (car lst))
        (else
         (let* ((lst lst)
                (listatpos (map (lambda (vec) (vector-ref vec pos)) lst))
                (num (criteria listatpos))
                (lst (filter
                        (lambda (x) (eqv? num (vector-ref x pos)))
                        lst)))
           (rec-bit-criteria-filter lst (+ pos 1))))))
   (rec-bit-criteria-filter lst 0))

(define (count-zeros-and-ones lst)
   (define (rec-count-zeros-and-ones lst zeros ones)
     (cond
      ((eq? lst '()) (cons zeros ones))
      (else
       (let* ((num (car lst))
              (zeros (if (eq? num 0) (+ 1 zeros) zeros))
              (ones (if (eq? num 1) (+ 1 ones) ones)))
         (rec-count-zeros-and-ones (cdr lst) zeros ones)))))
   (rec-count-zeros-and-ones lst 0 0))

(define (most-common lst)
  (let* ((count (count-zeros-and-ones lst))
         (zeros (car count))
         (ones (cdr count)))
   (if (>= ones zeros) 1 0)))

(define (least-common lst)
  ;; Note: if a number is not at all present it is considered the least
  ;; common anyway
  (let* ((count (count-zeros-and-ones lst))
         (zeros (car count))
         (ones (cdr count)))
   (if (<= zeros ones) 0 1)))

(define (vec-of-binary-digits->num vec)
  (let ((len (vector-length vec)))
    (define (rec-f acc pos)
     (begin
        (cond
         ((eq? pos len) acc)
         (else (rec-f
                 (+ (* 2 acc) (vector-ref vec pos))
                 (+ pos 1))))))
    (rec-f 0 0)))

(define (o2-generator-rating-vec lst)
  (bit-criteria-filter most-common lst))

(define (o2-generator-rating lst)
  (vec-of-binary-digits->num
    (o2-generator-rating-vec lst)))

(define (co2-scrubber-rating-vec lst)
  (bit-criteria-filter least-common lst))

(define (co2-scrubber-rating lst)
  (vec-of-binary-digits->num
    (co2-scrubber-rating-vec lst)))

(define (life-support-rating)
  (let* ((input (stream-of-lines))
         (input (stream-map string->list input))
         (char0 (char->integer #\0))
         (input (stream-map
                  (lambda (line)
                    (map
                      (lambda (char)
                          (- (char->integer char) char0))
                      line))
                  input))
         (input (stream-map list->vector input))
         (input-lst (stream->list input))
         (o2-gen (o2-generator-rating input-lst))
         (co2-scrub (co2-scrubber-rating input-lst)))
    (* o2-gen co2-scrub)))

(define-public (main args)
  (let ((result (life-support-rating)))
   (format #t "result is: ~d" result)))
