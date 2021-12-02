(define-module (adventofcode second)
    #:use-module (ice-9 rdelim))

(define (anyfalse a b c)
    (or
     (equal? a #f)
     (equal? b #f)
     (equal? c #f)))

(define (count-increments acc p1 p2 p3)
    (let ((line (read-line)))
      (cond
       ((eof-object? line)
        ;; end of file, return final result
        acc)
       (else (let ((num (string->number line)))
                 (cond
                  ((equal? num #f)
                   ;; not a number, ignore
                   (count-increments acc last))
                  ((anyfalse p1 p2 p3)
                   ;; lacking at least one measurment
                   (count-increments acc p2 p3 num))
                  ((> (+ p2 p3 num) (+ p1 p2 p3))
                   ;; detected increment, increase accumulator
                   (count-increments (+ acc 1) p2 p3 num))
                  (#t
                    ;; else, no increment just parse next line
                    (count-increments acc p2 p3 num))))))))

(display (count-increments 0 #f #f #f))
