(define-module (adventofcode2021 day22 tests)
    #:use-module (adventofcode2021 day22 utils)
    #:use-module (adventofcode2021 day22 reactor)
    #:use-module (ice-9 rdelim)
    #:use-module (srfi srfi-1)
    #:use-module (srfi srfi-2) ; and-let*
    #:use-module (srfi srfi-19)
    #:use-module (srfi srfi-41)
    #:use-module (srfi srfi-43)
    #:use-module (srfi srfi-64))

(define (dbg t v) (format #t "~s: ~a\n" t v) (force-output))

(define-syntax-rule (import-private module)
    (define module (@@ (adventofcode2021 day22 reactor) module)))

(define (guess-current-dirname)
  (and-let* ((source-location (current-source-location))
             (filename (assq-ref source-location 'filename))
             (absolute-filepath (%search-load-path filename))
             (absolute-dirname (dirname absolute-filepath)))
         absolute-dirname))

(define testdir (or (guess-current-dirname) (getcwd)))

;; import internal functions of module to test
(import-private make-cuboid)
(import-private parse-reboot-step)
(import-private coord-list->cuboid)

(test-begin "parse-reboot-step")
(test-equal '( "A" "B" "CC")
            (string-split-string "A..B..CC" ".."))
(test-equal (make-cuboid 10 12 10 12 10 12 #t)
            (parse-reboot-step "on x=10..12,y=10..12,z=10..12"))
(test-equal (make-cuboid 11 13 11 13 11 13 #t)
            (parse-reboot-step  "on x=11..13,y=11..13,z=11..13"))
(test-equal (make-cuboid 9 11 9 11 9 11 #f)
            (parse-reboot-step  "off x=9..11,y=9..11,z=9..11"))
(test-end "parse-reboot-step")

(test-begin "coord-list->cuboid")
(test-equal (make-cuboid 1 2 3 4 5 6 #t)
            (coord-list->cuboid '((1 . 2) (3 . 4) (5 . 6))))
(test-begin "coord-list->cuboid")

