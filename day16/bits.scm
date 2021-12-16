(define-module (adventofcode2021 day16 bits)
    #:use-module (adventofcode2021 day16 utils)
    #:use-module (ice-9 rdelim)
    #:use-module (ice-9 match)
    #:use-module (ice-9 format)
    #:use-module (srfi srfi-1) ; lists library
    #:use-module (srfi srfi-2) ; and-let*
    #:use-module (srfi srfi-11) ; let*-values
    #:use-module (srfi srfi-9)
    #:use-module (srfi srfi-9 gnu) ; immutable records
    #:use-module (srfi srfi-41) ; Streams
    #:use-module (srfi srfi-43)) ; Vectors iterators

(use-modules (ice-9 rdelim))
(use-modules (ice-9 match))
(use-modules (ice-9 format))
(use-modules (srfi srfi-2)) ; and-let*
(use-modules (srfi srfi-1)) ; lists library
(use-modules (srfi srfi-11)) ; let*-values
(use-modules (srfi srfi-9))
(use-modules (srfi srfi-9 gnu)) ; immutable records
(use-modules (srfi srfi-41))

(define (dbg t v) (format #t "~a~a\n" t v) (force-output))

(define (stream-carnumber strm l)
   (let loop ((strm strm) (l l) (acc 0))
       (cond
        ((stream-null? strm) (cons strm acc))
        ((<= l 0) (cons strm acc))
        (else (let* ((digit (stream-car strm))
                     (digit (- (char->integer digit) (char->integer #\0)))
                     (strm (stream-cdr strm))
                     (acc (+ (* acc 2) digit)))
                (loop strm (1- l) acc))))))

(define-record-type <packet-result>
  (make-result strm packet-length sumversion other)
  packet-result?
  (strm result-strm)
  (packet-length result-packet-length)
  (sumversion result-sumversion)
  (other result-other))
(define (result-length+ num result)
   (make-result
      (result-strm result)
      (+ (result-packet-length result) num)
      (result-sumversion result)
      (result-other result)))

(define (stream->packet-result strm)
   (define (parse-packet strm versionsum)
     (let* ((version (stream-carnumber strm 3))
            (strm (car version))
            (version (cdr version))
            (type (stream-carnumber strm 3))
            (strm (car type))
            (type (cdr type)))
        (cond
         ((= type 4)
          (result-length+ 6 ; version+type take 6 bits
                    (parse-literal strm (+ versionsum version))))
         (else
          (result-length+ 6 ; version+type take 6 bits
                    (parse-operator type strm (+ versionsum version)))))))
   (define (parse-literal strm sumversion)
    (let loop ((strm strm) (acc 0) (literal-length 0))
     (let* ((continuation-bit (stream-carnumber strm 1))
            (strm (car continuation-bit))
            (continuation-bit (cdr continuation-bit))
            (val (stream-carnumber strm 4))
            (strm (car val))
            (val (cdr val))
            (val (+ val (* 16 acc))) ; I calculate val but never uses it
            (literal-length (+ 5 literal-length)))
         (cond
          ((= 1 continuation-bit)
           (loop strm val literal-length))
          (else
           ;;(format #t "literal=~a\n" val)
           (make-result strm  literal-length sumversion val))))))
   (define (parse-operator type strm sumversion)
      (let* ((length-type (stream-carnumber strm 1))
             (strm (car length-type))
             (length-type (cdr length-type)))
        (cond
          ((= length-type 1) ; 11 next bits are number of subpackets
           (let* ((subpacketsnum (stream-carnumber strm 11))
                  (strm (car subpacketsnum))
                  (subpacketsnum (cdr subpacketsnum)))
             (let nextsubpackets ((numtoparse subpacketsnum) (strm strm) (sumversion sumversion) (current-length (+ 1 11)) (current-subvalues '()))
                   (cond
                     ((<= numtoparse 0) (make-result strm current-length sumversion (operation-apply type current-subvalues)))
                     (else (let* ((numtoparse     (1- numtoparse))
                                  (subresult      (parse-packet strm sumversion))
                                  (strm           (result-strm subresult))
                                  (current-length (+ current-length (result-packet-length subresult)))
                                  (sumversion     (result-sumversion subresult))
                                  (current-subvalues (cons (result-other subresult) current-subvalues)))
                              (nextsubpackets numtoparse strm sumversion current-length current-subvalues)))))))
          (else ; length-type == 0, 15 next bits are the length of subpackets
            (let* ((subpacketslength (stream-carnumber strm 15))
                   (strm (car subpacketslength))
                   (subpacketslength (cdr subpacketslength)))
             (let nextsubpackets ((lengthtoparse subpacketslength) (strm strm) (sumversion sumversion) (current-length 0) (current-subvalues '()))
                   (cond
                     ((<= lengthtoparse 0)
                      (if (not (= current-length subpacketslength))
                          (format (current-error-port) "ERROR: length do not match ~a vs. ~a" current-length subpacketslength))
                      (make-result strm (+ 1 15 current-length) sumversion (operation-apply type current-subvalues)))
                     (else (let* (
                                  (subresult         (parse-packet strm sumversion))
                                  (strm              (result-strm subresult))
                                  (current-length    (+ current-length (result-packet-length subresult)))
                                  (lengthtoparse     (- lengthtoparse (result-packet-length subresult)))
                                  (sumversion        (result-sumversion subresult))
                                  (current-subvalues (cons (result-other subresult) current-subvalues)))
                              (nextsubpackets lengthtoparse strm sumversion current-length current-subvalues))))))))))
   (parse-packet strm 0))

(define (operation-apply type subvalues)
 (cond
    ((= 0 type) (apply + subvalues))
    ((= 1 type) (apply * subvalues))
    ((= 2 type) (apply min subvalues))
    ((= 3 type) (apply max subvalues))
    ((= 5 type) (cond ((> (cadr subvalues) (car subvalues)) 1) ; TODO: explain this
                      (else 0)))
    ((= 6 type) (cond ((< (cadr subvalues) (car subvalues)) 1) ; TODO: explain this
                      (else 0)))
    ((= 7 type) (cond ((= (cadr subvalues) (car subvalues)) 1) ; TODO: explain this
                      (else 0)))))

(define-public (main args)
   (let* ((strm-char (stream-of-chars (current-input-port)))
          (stream-bits (stream-of-bits strm-char))
          (packet-result (stream->packet-result stream-bits))
          (result1 (result-sumversion packet-result))
          (result2 (result-other packet-result)))
     (format #t "result1: ~a\n" result1)
     (format #t "result2: ~a\n" result2)))
