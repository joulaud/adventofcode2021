(define-module (adventofcode2021 day18 tests)
    #:use-module (adventofcode2021 day18 utils)
    #:use-module (adventofcode2021 day18 snailfish)
    #:use-module (ice-9 rdelim)
    #:use-module (srfi srfi-1)
    #:use-module (srfi srfi-2) ; and-let*
    #:use-module (srfi srfi-19)
    #:use-module (srfi srfi-41)
    #:use-module (srfi srfi-43)
    #:use-module (srfi srfi-64))

(define (dbg t v) (format #t "~s: ~a\n" t v) (force-output))

(define-syntax-rule (import-private module)
    (define module (@@ (adventofcode2021 day18 snailfish) module)))

(define (guess-current-dirname)
  (and-let* ((source-location (current-source-location))
             (filename (assq-ref source-location 'filename))
             (absolute-filepath (%search-load-path filename))
             (absolute-dirname (dirname absolute-filepath)))
         absolute-dirname))

(define testdir (or (guess-current-dirname) (getcwd)))

;; import internal functions of module to test
(import-private list->snailfish)
(import-private snailfish-add)
(import-private snailfish-explode)
(import-private snailfish-split)
(import-private snailfish-reduce)

(test-begin "add")
(test-equal
  (list->snailfish '((1 . 2) . ((3 . 4) . 5)))
  (snailfish-add (list->snailfish '(1 . 2))
                 (list->snailfish '((3 . 4) . 5))))
(test-end "add")

(test-begin "explode")
(test-equal
          (list->snailfish '((((0 . 9) . 2) . 3) . 4))
          (cdr (snailfish-explode (list->snailfish '(((((9 . 8) . 1) . 2) . 3) . 4)))))
(test-equal
          (list->snailfish '(7 . (6 . (5 . (7 . 0)))))
          (cdr (snailfish-explode (list->snailfish '(7 . (6 . (5 . (4 . (3 . 2)))))))))
(test-equal
          (list->snailfish '((6 . (5 . (7 . 0))) . 3))
          (cdr (snailfish-explode (list->snailfish '((6 . (5 . (4 . (3 . 2)))) . 1)))))
(test-equal
          (list->snailfish '((3 . (2 . (8 . 0))) . (9 . (5 . (4 . (3 . 2))))))
          (cdr (snailfish-explode (list->snailfish '((3 . (2 . (1 . (7 . 3)))) . (6 . (5 . (4 . (3 . 2)))))))))
(test-equal
          (list->snailfish '((3 . (2 . (8 . 0))) . (9 . (5 . (7 . 0)))))
          (cdr (snailfish-explode (list->snailfish '((3 . (2 . (8 . 0))) . (9 . (5 . (4 . (3 . 2)))))))))
;; no explosion when no more than 3 level of nesting
(test-equal
          (list->snailfish '(1 . (2 . (3 . 4))))
          (cdr (snailfish-explode (list->snailfish '(1 . (2 . (3 . 4)))))))
(test-end "explode")
