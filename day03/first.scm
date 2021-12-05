(define-module (first)
    #:use-module (ice-9 rdelim)
    #:use-module (ice-9 match)
    #:use-module (srfi srfi-9)
    #:use-module (srfi srfi-9 gnu) ; immutable records
    #:use-module (srfi srfi-41))

(define stream-of-lines
  (stream-lambda ()
     (let
         ((line (read-line)))
      (cond
       ((eof-object? line) stream-null)
       (else (stream-cons line (stream-of-lines)))))))

(define-immutable-record-type <binary-counter>
    (make-binary-counter zeros ones)
    binary-counter?
    (zeros count0 set-count0)
    (ones count1 set-count1))

(define binary-counter-empty (make-binary-counter 0 0))

(define (inc0 counter)
    (set-count0
     counter
     (+ 1 (count0 counter))))

(define (inc1 counter)
    (set-count1
     counter
     (+ 1 (count1 counter))))

(define (add-char-to-counter counter c)
    (cond
     ((char=? #\0 c) (inc0 counter))
     ((char=? #\1 c) (inc1 counter))))

(define list-of-5-binary-counter-empty
    (list
     binary-counter-empty
     binary-counter-empty
     binary-counter-empty
     binary-counter-empty
     binary-counter-empty))

(define (add-to-list-of-binary-counter acc cur)
    (map add-char-to-counter acc cur))

(define (count-binary-in-diagnostic-stream strm)
   (stream-fold
    add-to-list-of-binary-counter
    list-of-5-binary-counter-empty
    strm))

(define-public (main args)
  (format #t "UNIMPLEMENTED"))
