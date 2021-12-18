(define-module (adventofcode2021 day17 tests)
    #:use-module (adventofcode2021 day17 utils)
    #:use-module (adventofcode2021 day17 trickshot)
    #:use-module (ice-9 rdelim)
    #:use-module (srfi srfi-1)
    #:use-module (srfi srfi-2) ; and-let*
    #:use-module (srfi srfi-19)
    #:use-module (srfi srfi-41)
    #:use-module (srfi srfi-43)
    #:use-module (srfi srfi-64))

(define (dbg t v) (format #t "~s: ~a\n" t v) (force-output))

(define-syntax-rule (import-private module)
    (define module (@@ (adventofcode2021 day17 trickshot) module)))

(define (guess-current-dirname)
  (and-let* ((source-location (current-source-location))
             (filename (assq-ref source-location 'filename))
             (absolute-filepath (%search-load-path filename))
             (absolute-dirname (dirname absolute-filepath)))
         absolute-dirname))

(define testdir (or (guess-current-dirname) (getcwd)))

(define example-string
 "target area: x=20..30, y=-10..-5")

;; import internal functions of module to test
(import-private string->target)
(import-private make-target)

(test-begin "parsing")
(test-equal (make-target 20 30 -10 -5)
   (string->target example-string))
(test-end "parsing")

;; import internal functions of module to test
(import-private search-y-max)

(test-begin "find-vy-init-max")
(test-equal 45 (search-y-max (make-target 20 30 -10 -5)))
(test-end "find-vy-init-max")
