(define-module (adventofcode2021 day16 tests)
    #:use-module (adventofcode2021 day16 utils)
    #:use-module (adventofcode2021 day16 bits)
    #:use-module (ice-9 rdelim)
    #:use-module (srfi srfi-1)
    #:use-module (srfi srfi-2) ; and-let*
    #:use-module (srfi srfi-19)
    #:use-module (srfi srfi-41)
    #:use-module (srfi srfi-43)
    #:use-module (srfi srfi-64))

(define (dbg t v) (format #t "~s: ~a\n" t v) (force-output))

(define-syntax-rule (import-private module)
    (define module (@@ (adventofcode2021 day16 bits) module)))

(define (guess-current-dirname)
  (and-let* ((source-location (current-source-location))
             (filename (assq-ref source-location 'filename))
             (absolute-filepath (%search-load-path filename))
             (absolute-dirname (dirname absolute-filepath)))
         absolute-dirname))

(define testdir (or (guess-current-dirname) (getcwd)))

(define test-strings
 '(
   "8A004A801A8002F478"
   "620080001611562C8802118E34"
   "C0015000016115A2E0802F182340"
   "A0016C880162017C3686B18A3D4780"))

(define tests-sum-versions
 '(16
   12
   23
   31))

;; import internal functions of module to test
(import-private stream->packet-result)
(import-private result-sumversion)

(test-begin "parsing input")
(define (string->sumversion str)
   (let* ((strm-char (stream-of-chars (open-input-string str)))
          (stream-bits (stream-of-bits strm-char))
          (packet-result (stream->packet-result stream-bits))
          (sumversion (result-sumversion packet-result)))
     sumversion))
(test-equal (car tests-sum-versions) (string->sumversion (car test-strings)))
(test-equal (cadr tests-sum-versions) (string->sumversion (cadr test-strings)))
(test-equal (caddr tests-sum-versions) (string->sumversion (caddr test-strings)))
(test-end "parsing input")

;; packet
;; 0123456789ABCDEF
;
;; T=4 => literal
;; VVVTTTNNNNN0NNNN
;; VVV1001NNNN0NNNN
;;
;; T!=4 => operator
;; VVVTTTI
