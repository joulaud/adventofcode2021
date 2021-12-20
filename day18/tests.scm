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
          (cons #t (list->snailfish '((((0 . 9) . 2) . 3) . 4)))
          (snailfish-explode (list->snailfish '(((((9 . 8) . 1) . 2) . 3) . 4))))
(test-equal
          (cons #t (list->snailfish '(7 . (6 . (5 . (7 . 0))))))
          (snailfish-explode (list->snailfish '(7 . (6 . (5 . (4 . (3 . 2))))))))
(test-equal
          (cons #t (list->snailfish '((6 . (5 . (7 . 0))) . 3)))
          (snailfish-explode (list->snailfish '((6 . (5 . (4 . (3 . 2)))) . 1))))
(test-equal
          (cons #t (list->snailfish '((3 . (2 . (8 . 0))) . (9 . (5 . (4 . (3 . 2)))))))
          (snailfish-explode (list->snailfish '((3 . (2 . (1 . (7 . 3)))) . (6 . (5 . (4 . (3 . 2))))))))
(test-equal
          (cons #t (list->snailfish '((3 . (2 . (8 . 0))) . (9 . (5 . (7 . 0))))))
          (snailfish-explode (list->snailfish '((3 . (2 . (8 . 0))) . (9 . (5 . (4 . (3 . 2))))))))
;; no explosion when no more than 3 level of nesting
(test-equal
          (cons #f (list->snailfish '(1 . (2 . (3 . 4)))))
          (snailfish-explode (list->snailfish '(1 . (2 . (3 . 4))))))
(test-end "explode")

(test-begin "split")
(test-equal
  (cons #t (list->snailfish '(((((4 . 3) . 4) . 4) . (7 . ((8 . 4) . 9))) . (1 . 1))))
  (snailfish-split (list->snailfish '((((0 . 7) . 4) . ((7 . 8) . (0 . (6 . 7)))) . (1 . 1)))))
(test-end "split")


(test-begin "reduce")
(test-equal
  (list->snailfish '((((0 . 7) . 4) . ((7 . 8) . (6 . 0))) . (8 . 1)))
  (snailfish-reduce (list->snailfish '(((((4 . 3) . 4) . 4) . (7 . ((8 . 4) . 9))) . (1 . 1)))))
(test-end "reduce")


(import-private read-snailfish)
(import-private read-snailfishes)
(import-private snailfish-full-add-lst)
(import-private snailfish-full-add)

(test-begin "parsing")
(test-equal (list->snailfish '(1 . 2))
            (read-snailfish (open-input-string "[1,2]")))
(test-equal (list->snailfish '(1 . (2 . 3)))
            (read-snailfish (open-input-string "[1,[2,3]]")))
(test-end "parsing")


(test-begin "complete-add")
(define (string->snailfish str)
   (read-snailfish (open-input-string str)))
(define (string->snailfishes str)
   (read-snailfishes (open-input-string str)))
(test-equal  (string->snailfish "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]")
     (snailfish-full-add
       (string->snailfish "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]")
       (string->snailfish "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]")))
(test-end "complete-add")

(test-begin "complete-add-lst")
(test-equal (string->snailfish "[[[[1,1],[2,2]],[3,3]],[4,4]]")
            (snailfish-full-add-lst (string->snailfishes "[1,1]
[2,2]
[3,3]
[4,4]")))

(test-equal (string->snailfish "[[[[3,0],[5,3]],[4,4]],[5,5]]")
            (snailfish-full-add-lst (string->snailfishes "[1,1]
[2,2]
[3,3]
[4,4]
[5,5]")))

(test-equal (string->snailfish "[[[[5,0],[7,4]],[5,5]],[6,6]]")
            (snailfish-full-add-lst (string->snailfishes "[1,1]
[2,2]
[3,3]
[4,4]
[5,5]
[6,6]")))

(snailfish-full-add-lst (string->snailfishes "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
[7,[5,[[3,8],[1,4]]]]
[[2,[2,2]],[8,[8,1]]]
[2,9]
[1,[[[9,3],9],[[9,0],[0,7]]]]
[[[5,[7,4]],7],1]
[[[[4,2],2],6],[8,7]]"))
(test-equal (string->snailfish "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]")
            (snailfish-full-add-lst (string->snailfishes "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
[7,[5,[[3,8],[1,4]]]]
[[2,[2,2]],[8,[8,1]]]
[2,9]
[1,[[[9,3],9],[[9,0],[0,7]]]]
[[[5,[7,4]],7],1]
[[[[4,2],2],6],[8,7]]")))

(test-end "complete-add-lst")

(import-private snailfish-magnitude)

(test-begin "magnitudes")
(snailfish-magnitude (string->snailfish "[[1,2],[[3,4],5]]"))
(test-equal 143
            (car (snailfish-magnitude (string->snailfish "[[1,2],[[3,4],5]]"))))
(test-equal 1384
            (car (snailfish-magnitude (string->snailfish "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"))))
(test-equal 445
            (car (snailfish-magnitude (string->snailfish "[[[[1,1],[2,2]],[3,3]],[4,4]]"))))
(test-equal 791
            (car (snailfish-magnitude (string->snailfish "[[[[3,0],[5,3]],[4,4]],[5,5]]"))))
(test-equal 1137
            (car (snailfish-magnitude (string->snailfish "[[[[5,0],[7,4]],[5,5]],[6,6]]"))))
(test-equal 3488
            (car (snailfish-magnitude (string->snailfish "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"))))
(test-end "magnitudes")

