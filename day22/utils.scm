(define-module (adventofcode2021 day22 utils)
    #:use-module (ice-9 rdelim) ; read-line
    #:use-module (ice-9 match)
    #:use-module (ice-9 format)
    #:use-module (srfi srfi-1) ; lists library
    #:use-module (srfi srfi-2) ; and-let*
    #:use-module (srfi srfi-11) ; let*-values
    #:use-module (srfi srfi-9)
    #:use-module (srfi srfi-9 gnu) ; immutable records
    #:use-module (srfi srfi-41) ; Streams
    #:use-module (srfi srfi-43)) ; Vectors iterators

(use-modules (srfi srfi-41)) ; Streams
(use-modules (ice-9 rdelim))
(define-public (dbg t v) (format #t "~s: ~a\n" t v) (force-output))


(define-public stream-of-chars (stream-lambda (port) (let ((char (read-char port))) (cond ((eof-object? char) stream-null) (else (stream-cons char (stream-of-chars port))))))) 
(define (hex->bits hex)
  (string->list
     (cond
       ((char=? #\0 hex) "0000")
       ((char=? #\1 hex) "0001")
       ((char=? #\2 hex) "0010")
       ((char=? #\3 hex) "0011")
       ((char=? #\4 hex) "0100")
       ((char=? #\5 hex) "0101")
       ((char=? #\6 hex) "0110")
       ((char=? #\7 hex) "0111")
       ((char=? #\8 hex) "1000")
       ((char=? #\9 hex) "1001")
       ((char=? #\A hex) "1010")
       ((char=? #\B hex) "1011")
       ((char=? #\C hex) "1100")
       ((char=? #\D hex) "1101")
       ((char=? #\E hex) "1110")
       ((char=? #\F hex) "1111")
       (else ""))))

(define-public stream-of-bits
  (stream-lambda (strm)
    (cond ((stream-null? strm) strm)
          (else (let* ((hex (stream-car strm))
                       (bits (hex->bits hex)))
                  (stream-append (list->stream bits) (stream-of-bits (stream-cdr strm))))))))

(define-public stream-of-lines
  (stream-lambda (port)
     (let
         ((line (read-line port)))
      (cond
       ((eof-object? line) stream-null)
       (else (stream-cons line (stream-of-lines port)))))))

(define-public (read-bloc port)
  (let loop ((line (read-line port)) (out '()))
       (cond
         ((eof-object? line) (reverse out))
         ((string-null? line) (reverse out))
         (else (loop (read-line port) (cons line out))))))

(define-public (array-for-each-index a proc-cell proc-endofline)
  (let* ((dims (array-dimensions a))
         (maxl (car dims))
         (maxc (cadr dims)))
    (do ((i 0 (1+ i)))
        ((>= i maxl))
      (do ((j 0 (1+ j)))
          ((>= j maxc))
        (proc-cell i j (array-ref a i j)))
      (proc-endofline i))))

(define-public (array-for-each-only-index a proc-cell)
  (let* ((dims (array-dimensions a))
         (maxl (car dims))
         (maxc (cadr dims)))
    (do ((i 0 (1+ i)))
        ((>= i maxl))
      (do ((j 0 (1+ j)))
          ((>= j maxc))
        (proc-cell i j)))))

(define-public (print-array a)
  (array-for-each-index
       a
       (lambda (i j v) (if v (display "X") (display ".")))
       (lambda (i) (display "\n"))))

(define (line->pair line)
   (let* (
          (x (string-split line #\-))
          (x (cons (car x) (cadr x))))
     x))

(define-public (line->numlist line)
   (let* (
          (x (string->list line))
          (x (map char->integer x))
          (char0 (char->integer #\0))
          (x (map (lambda (v) (- v char0)) x)))
     x))

(define-public (stream-of-lines->array line->list strm)
   (let* ((x (stream-map line->list strm))
          (x (stream->list x))
          (x (list->array 2 x)))
     x))


;; copy-paste from https://stackoverflow.com/questions/5546552/scheme-recursive-function-to-compute-all-possible-combinations-of-some-lists/5547174#5547174
(define (concat/map ls f)
    (cond
      ((null? ls) '())
      (else (append (f (car ls)) (concat/map (cdr ls) f)))))

(define (combine xs ys)
    (concat/map xs (lambda (x)
                     (map (lambda (y) (list x y)) ys))))

(define-public (combine* xs . ys*)
    (cond
      [(null? ys*) (map list xs)]
      [(null? (cdr ys*)) (combine xs (car ys*))]
      [else (concat/map xs (lambda (x)
                             (map (lambda (y) (cons x y))
                                  (apply combine* ys*))))]))

(define-public (string-split-string str spl)
 (let ((spl-l (string-length spl)))
   (let string-split-string-rec ((index 0) (result '()))
     (let* ((spl-index (string-contains str spl index)))
       (cond
        (spl-index (let* ((next-index (+ spl-index spl-l))
                          (extracted (substring str index spl-index)))
                       (string-split-string-rec next-index (cons extracted result))))
        (else (reverse (cons (substring str index)
                             result))))))))


(define-public (fold-three-combinations proc init l1 l2 l3)
   (let fold-rec-1 ((l1 l1) (result init))
      (dbg "l1:" (length l1))
      (cond
       ((null? l1) result)
       (else (let* ((c1 (car l1))
                    (result (fold-sub-two-combinations proc result c1 l2 l3)))
               (fold-rec-1 (cdr l1) result))))))

(define (fold-sub-two-combinations proc init c1 l2 l3)
   (let fold-rec-2 ((l2 l2) (result init))
      (cond
       ((null? l2) result)
       (else (let* ((c2 (car l2))
                    (result (fold-sub-one-combinations proc result c1 c2 l3)))
               (fold-rec-2 (cdr l2) result))))))

(define (fold-sub-one-combinations proc init c1 c2 l3)
   (let fold-rec-3 ((l3 l3) (result init))
      (cond
       ((null? l3) result)
       (else (let* ((c3 (car l3))
                    (result (proc c1 c2 c3 result)))
               (fold-rec-3 (cdr l3) result))))))


