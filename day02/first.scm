(define-module (adventofcode secondwithstreams)
    #:use-module (ice-9 rdelim)
    #:use-module (ice-9 match)
    #:use-module (srfi srfi-9)
    #:use-module (srfi srfi-41))

(define stream-of-lines
  (stream-lambda ()
     (let
         ((line (read-line)))
      (cond
       ((eof-object? line) stream-null)
       (else (stream-cons line (stream-of-lines)))))))

(define-record-type <position>
  (make-position horiz depth)
  position?
  (horiz position-horiz)
  (depth position-depth))

(define pos0 (make-position 0 0))

(define (forward x)
  (lambda (pos)
    (match pos
           (($ <position> horiz depth)
            (make-position (+ horiz x) depth)))))

(define (down x)
  (lambda (pos)
    (match pos
           (($ <position> horiz depth)
            (make-position horiz (+ depth x))))))

(define (up x)
  (lambda (pos)
    (match pos
           (($ <position> horiz depth)
            (make-position horiz (- depth x))))))

(define (line->command line)
    ;;forward X increases the horizontal position by X units.
    ;;down X increases the depth by X units.
    ;;up X decreases the depth by X units.
   (let* ((splitted (string-split line #\space))
          (cmd (car splitted))
          (val (cadr splitted)))
     (cond
        ((string=? "forward" cmd)
         (forward (string->number val)))
        ((string=? "down" cmd)
         (down (string->number val)))
        ((string=? "up" cmd)
         (up (string->number val))))))

(let* ((x pos0)
       (x ((line->command "forward 5") x))
       (x ((line->command "down 5") x))
       (x ((line->command "forward 8") x))
       (x ((line->command "up 3") x))
       (x ((line->command "down 8") x))
       (x ((line->command "forward 2") x)))
    x)


(define (move-submarine stream-of-lines)
    (let* ((x stream-of-lines)
           (x (stream-map line->command x))
           (x (stream-fold
                (lambda (pos cur)
                    (cur pos))
                pos0
                x)))
     x))

(define testentry (stream
                   "forward 5"
                   "down 5"
                   "forward 8"
                   "up 3"
                   "down 8"
                   "forward 2"))

(format #t "~a\n"
   (match
      (move-submarine testentry)
      (($ <position> horiz depth)
       (* horiz depth))))

(format #t "~a\n"
         (match
          (move-submarine (stream-of-lines))
          (($ <position> horiz depth)
           (* horiz depth))))
