(define-module (adventofcode2021 day13 tests)
    #:use-module (adventofcode2021 day13 utils)
    #:use-module (adventofcode2021 day13 origami)
    #:use-module (ice-9 rdelim)
    #:use-module (srfi srfi-1)
    #:use-module (srfi srfi-2) ; and-let*
    #:use-module (srfi srfi-19)
    #:use-module (srfi srfi-41)
    #:use-module (srfi srfi-43)
    #:use-module (srfi srfi-64))

(define (dbg t v) (format #t "~s: ~a\n" t v) (force-output))

(define-syntax-rule (import-private module)
    (define module (@@ (adventofcode2021 day13 origami) module)))

(define (guess-current-dirname)
  (and-let* ((source-location (current-source-location))
             (filename (assq-ref source-location 'filename))
             (absolute-filepath (%search-load-path filename))
             (absolute-dirname (dirname absolute-filepath)))
         absolute-dirname))

(define testdir (or (guess-current-dirname) (getcwd)))

(define example-printed "...#..#..#.
....#......
...........
#..........
...#....#.#
...........
...........
...........
...........
...........
.#....#.##.
....#......
......#...#
#..........
#.#........
")

(define example-first-fold "#.##..#..#.
#...#......
......#...#
#...#......
.#.#..#.###
...........
...........")

;; import internal functions of module to test
(import-private line->point)
(import-private make-point)
(import-private lines->sheet)
(import-private sheet->string)

(test-begin "parsing input")
(test-equal (make-point 6 10) (line->point "6,10"))
(define example-sheet
    (lines->sheet
       (read-bloc (open-input-file (string-append testdir "/inputs/example.txt")))))
(test-equal example-printed
            (sheet->string example-sheet))
(test-end "parsing input")


(import-private point-fold-vert)
(import-private point-fold-horiz)
(import-private sheet-fold)
(test-begin "folding")
(test-equal (make-point 0 0) (point-fold-vert 5 (make-point 10 0)))
(test-equal (make-point 0 0) (point-fold-horiz 5 (make-point 0 10)))
(test-end "folding")
