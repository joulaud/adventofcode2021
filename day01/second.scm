(define-module (adventofcode first)
    #:use-module (ice-9 rdelim))

(define (count-increments acc last)
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
                  ((equal? last #f)
                   ;; no previous measurements
                   (count-increments acc num))
                  ((> num last)
                   ;; detected increment, increase accumulator
                   (count-increments (+ acc 1) num))
                  (#t
                    ;; else, no increment just parse next line
                    (count-increments acc num))))))))

(display (count-increments 0 #f))
