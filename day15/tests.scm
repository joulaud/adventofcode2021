(define-module (adventofcode2021 day15 tests)
    #:use-module (adventofcode2021 day15 utils)
    #:use-module (adventofcode2021 day15 escapepath)
    #:use-module (ice-9 rdelim)
    #:use-module (srfi srfi-1)
    #:use-module (srfi srfi-2) ; and-let*
    #:use-module (srfi srfi-19)
    #:use-module (srfi srfi-41)
    #:use-module (srfi srfi-43)
    #:use-module (srfi srfi-64))

(define (dbg t v) (format #t "~s: ~a\n" t v) (force-output))

(define-syntax-rule (import-private module)
    (define module (@@ (adventofcode2021 day15 escapepath) module)))

(define (guess-current-dirname)
  (and-let* ((source-location (current-source-location))
             (filename (assq-ref source-location 'filename))
             (absolute-filepath (%search-load-path filename))
             (absolute-dirname (dirname absolute-filepath)))
         absolute-dirname))

(define testdir (or (guess-current-dirname) (getcwd)))

(define example-string
  "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581")

;; import internal functions of module to test
(import-private read-cavemap)
(import-private cavemap-ref)
(import-private neighbours)
(import-private pair->coord)
(import-private make-coord)

(test-begin "parsing input")
(define example-cavemap (read-cavemap (open-input-string example-string)))
(test-equal 6 (cavemap-ref example-cavemap 0 2))
(test-equal 7 (cavemap-ref example-cavemap 4 0))
(test-end "parsing input")

(test-begin "neighbours")
(test-equal (map pair->coord '((1 . 0) (0 . 1)))
            (neighbours (make-coord 0 0) (make-coord 10 10)))
(test-equal (map pair->coord '((4 . 5) (6 . 5) (5 . 4) (5 . 6)))
            (neighbours (make-coord 5 5) (make-coord 10 10)))
(test-equal (map pair->coord '((9 . 10) (10 . 9)))
            (neighbours (make-coord 10 10) (make-coord 10 10)))
(test-end "neighbours")

;; import internal functions of module to test
(import-private lowest-risk)

(test-begin "lowest-risk")
(test-equal 2 (lowest-risk example-cavemap '() (make-coord 8 8) 0))
(test-equal 40 (lowest-risk example-cavemap '() (make-coord 0 0) 0))
test-end "lowest-risk"
