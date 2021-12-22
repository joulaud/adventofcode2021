(define-module (adventofcode2021 day20 tests)
    #:use-module (adventofcode2021 day20 utils)
    #:use-module (adventofcode2021 day20 trench)
    #:use-module (ice-9 rdelim)
    #:use-module (srfi srfi-1)
    #:use-module (srfi srfi-2) ; and-let*
    #:use-module (srfi srfi-19)
    #:use-module (srfi srfi-41)
    #:use-module (srfi srfi-43)
    #:use-module (srfi srfi-64))

(define (dbg t v) (format #t "~s: ~a\n" t v) (force-output))

(define-syntax-rule (import-private module)
    (define module (@@ (adventofcode2021 day20 trench) module)))

(define (guess-current-dirname)
  (and-let* ((source-location (current-source-location))
             (filename (assq-ref source-location 'filename))
             (absolute-filepath (%search-load-path filename))
             (absolute-dirname (dirname absolute-filepath)))
         absolute-dirname))

(define testdir (or (guess-current-dirname) (getcwd)))

;; import internal functions of module to test
(import-private parse-enhancement)
(import-private parse-image)
(import-private make-image)
(import-private make-point)
(import-private image->string)

(test-begin "parsing")
(test-equal '(#f #t #t) (parse-enhancement ".##"))
(test-equal (make-image (make-point 0 0) (make-point 1 1)
                        (list (cons (make-point 0 1) #t)
                              (cons (make-point 1 0) #t)))
            (parse-image (open-input-string ".#\n#.\n")))
(test-end "parsing")

(test-begin "printing")
(test-equal
  "....
..#.
.#..
....
"
  (image->string (parse-image (open-input-string ".#\n#.\n"))))
(test-end "printing")


(import-private image->string)
(import-private image-conversion-step)
(import-private image-points)
(import-private input-value)
(import-private output-pixel)

(test-begin "enhance-step")
(test-equal 34
           (input-value (make-point 1 1)
                        (image-points
                         (parse-image (open-input-string "...\n#..\n.#.")))))

(test-equal 73
            (input-value (make-point 1 1)
                        (image-points
                         (parse-image (open-input-string "..#\n..#\n..#")))))

(define enhancement (parse-enhancement
                     "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#"))
(test-equal #f (output-pixel  0 enhancement))
(test-equal #t (output-pixel 10 enhancement))
(test-equal #t (output-pixel 20 enhancement))
(test-equal #t (output-pixel 30 enhancement))
(test-equal #t (output-pixel 34 enhancement))
(test-equal #t (output-pixel 40 enhancement))
(test-equal #f (output-pixel 60 enhancement))
(define image (parse-image (open-input-string "#..#.
#....
##..#
..#..
..###")))

(test-equal 34
           (input-value (make-point 1 1) (image-points (parse-image (open-input-string "...
#..
.#.")))))

(test-equal
 "...........
...........
...##.##...
..#..#.#...
..##.#..#..
..####..#..
...#..##...
....##..#..
.....#.#...
...........
...........
"
 (image->string (image-conversion-step image enhancement)))
(test-end "enhance-step")

