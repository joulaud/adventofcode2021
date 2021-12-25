(define-module (adventofcode2021 day22 tests)
    #:use-module (adventofcode2021 day22 utils)
    #:use-module (adventofcode2021 day22 reactor)
    #:use-module (ice-9 rdelim)
    #:use-module (srfi srfi-1)
    #:use-module (srfi srfi-2) ; and-let*
    #:use-module (srfi srfi-19)
    #:use-module (srfi srfi-41)
    #:use-module (srfi srfi-43)
    #:use-module (srfi srfi-64)
    #:use-module (srfi srfi-11)) ; let*-values

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
(define example-reboot-steps-str "on x=10..12,y=10..12,z=10..12
on x=11..13,y=11..13,z=11..13
off x=9..11,y=9..11,z=9..11
on x=10..10,y=10..10,z=10..10")

;; import internal functions of module to test
(import-private make-cuboid)
(import-private parse-reboot-step)
(import-private parse-all-reboot-steps)
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
(define all-reboot-steps
                (parse-all-reboot-steps (open-input-string example-reboot-steps-str)))
(test-equal (list
             (make-cuboid 10 12 10 12 10 12 #t)
             (make-cuboid 11 13 11 13 11 13 #t)
             (make-cuboid 9 11 9 11 9 11 #f)
             (make-cuboid 10 10 10 10 10 10 #t))
            all-reboot-steps)
(test-end "parse-reboot-step")

(test-begin "coord-list->cuboid")
(test-equal (make-cuboid 1 2 3 4 5 6 #t)
            (coord-list->cuboid '((1 2) (3 4) (5 6))))
(test-end "coord-list->cuboid")

(test-begin "axis-intersect")
(import-private axis-intersect)

;; Disjoints
(let*-values
    (((both c1 c2) (axis-intersect 1 2 3 4)))
 (test-equal '() both)
 (test-equal '((1  . 2)) c1)
 (test-equal '((3  . 4)) c2))

;; Égaux
(let*-values
    (((both c1 c2) (axis-intersect 5 6 5 6)))
 (test-equal '((5  . 6)) both)
 (test-equal '() c1)
 (test-equal '() c2))

;; recouvrement à droite de c1
(let*-values
    (((both c1 c2) (axis-intersect 10 14 12 16)))
 (test-equal '((12 . 14)) both)
 (test-equal '((10 . 11)) c1)
 (test-equal '((15 . 16)) c2))

;; recouvrement à gauche de c1
(let*-values
    (((both c1 c2) (axis-intersect 12 16 10 14)))
 (test-equal '((12 . 14)) both)
 (test-equal '((15 . 16)) c1)
 (test-equal '((10 . 11)) c2))

;; c2 complètement inclus dans c1
(let*-values
    (((both c1 c2) (axis-intersect 20 26 22 24)))
 (test-equal '((22 . 24)) both)
 (test-equal '((20 . 21) (25 . 26)) c1)
 (test-equal '() c2))

;; c1 complètement inclus dans c2
(let*-values
    (((both c1 c2) (axis-intersect 22 24 20 26)))
 (test-equal '((22 . 24)) both)
 (test-equal '() c1)
 (test-equal '((20 . 21) (25 . 26)) c2))

(test-end "axis-intersect")


(test-begin "restrictions")
(import-private cuboid-restrict-x)
(import-private cuboid-restrict-y)
(test-equal
 (make-cuboid 12 14 20 22 42 48  #t)
 (cuboid-restrict-y
  (cuboid-restrict-x (make-cuboid 10 15 20 25 42 48  #t) 12 14)
  16 22))
(test-end "restrictions")

(test-begin "cuboid-size")
(import-private cuboid-size)
(import-private cuboids-size)
(import-private cuboids-size-bis)
(test-equal 1
            (cuboid-size (make-cuboid 1 1 4 4 6 6 #t)))
(test-equal 1000
            (cuboid-size (make-cuboid 1 10 31 40 51 60 #t)))
(test-equal 1
            (cuboid-size (make-cuboid -1 -1 -4 -4 -6 -6 #t)))

(test-equal (cuboids-size-bis (list (make-cuboid 1 1 4 4 6 6 #t)))
            (cuboid-size (make-cuboid 1 1 4 4 6 6 #t)))
(test-equal (cuboids-size-bis (list (make-cuboid 1 10 31 40 51 60 #t)))
            (cuboid-size (make-cuboid 1 10 31 40 51 60 #t)))

(test-equal 1001
            (cuboids-size (list (make-cuboid 1 10 31 40 51 60 #t)
                                (make-cuboid -1 -1 -4 -4 -6 -6 #t))))

(test-equal 1000
            (cuboids-size-bis (list (make-cuboid 1 10 31 40 51 60 #t)
                                    (make-cuboid 1 1 34 34 56 56 #t))))

(test-end "cuboid-size")



(test-begin "normalize-range-list")
(import-private make-range)
(import-private normalize-range-list)
(test-equal (list (make-range 0  1 #t) (make-range 1  2 #t)
                   (make-range 3  4 #t) (make-range 4  5 #t) (make-range 5  6 #t)
                   (make-range 10  11 #t) (make-range 11  14 #t) (make-range 14  15 #t)
                   (make-range 20  21 #t) (make-range 21  24 #t) (make-range 24  25 #t) (make-range 25  30 #t))
            (normalize-range-list (list (make-range 0  1 #t) (make-range 1  2 #t)
                                        (make-range 3  5 #t) (make-range 4  6 #t)
                                        (make-range 10  15 #t) (make-range 11  14 #t)
                                        (make-range 20  25 #t) (make-range 20  30 #t) (make-range 21  24 #t))))
(test-end "normalize-range-list")
