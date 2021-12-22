(define-module (adventofcode2021 day19 tests)
    #:use-module (adventofcode2021 day19 utils)
    #:use-module (adventofcode2021 day19 beacons)
    #:use-module (ice-9 rdelim)
    #:use-module (srfi srfi-1)
    #:use-module (srfi srfi-2) ; and-let*
    #:use-module (srfi srfi-19)
    #:use-module (srfi srfi-41)
    #:use-module (srfi srfi-43)
    #:use-module (srfi srfi-64))

(define (dbg t v) (format #t "~s: ~a\n" t v) (force-output))

(define-syntax-rule (import-private module)
    (define module (@@ (adventofcode2021 day19 beacons) module)))

(define (guess-current-dirname)
  (and-let* ((source-location (current-source-location))
             (filename (assq-ref source-location 'filename))
             (absolute-filepath (%search-load-path filename))
             (absolute-dirname (dirname absolute-filepath)))
         absolute-dirname))

(define testdir (or (guess-current-dirname) (getcwd)))

;; import internal functions of module to test
(import-private string->coord)
(import-private make-coord)
(import-private make-scan)
(import-private read-scanner)
(import-private read-all-scanners)
(import-private scan-beacons)

(test-begin "parsing")
(test-equal (make-coord -1 -1 1) (string->coord "-1,-1,1"))
(define simple-scanner "--- scanner 0 ---
-1,-1,1
-2,-2,2")
(test-equal (make-scan (list (make-coord -1 -1 1) (make-coord -2 -2 2)) "0")
            (car (read-scanner (open-input-string simple-scanner))))

(define two-scans-str "--- scanner 0 ---
404,-588,-901
528,-643,409
-838,591,734

--- scanner 1 ---
686,422,578
605,423,415")
(define two-scans (read-all-scanners (open-input-string two-scans-str)))
(test-equal 2 (length two-scans))
(test-equal 3 (length (scan-beacons (car two-scans))))
(test-equal (make-coord 404 -588 -901) (car (scan-beacons (car two-scans))))
(test-end "parsing")

(import-private coord<?)
(import-private zip-alists)
(import-private scan-pairs)
(import-private scan-give-transform)
(test-begin "XXX")
(define example-scans (read-all-scanners (open-input-file (string-append testdir "/inputs/example"))))
(define overlap-0-and-1-from0  (sort
                                (list
                                 (make-coord -618 -824 -621)
                                 (make-coord -537 -823 -458)
                                 (make-coord -447 -329 318)
                                 (make-coord 404  -588 -901)
                                 (make-coord 544  -627 -890)
                                 (make-coord 528  -643 409)
                                 (make-coord -661 -816 -575)
                                 (make-coord 390  -675 -793)
                                 (make-coord 423  -701 434)
                                 (make-coord -345 -311 381)
                                 (make-coord 459  -707 401)
                                 (make-coord -485 -357 347))
                                coord<?))

(define overlap-0-and-1-from1 (sort
                               (list
                                      (make-coord 686  422 578)
                                      (make-coord 605  423 415)
                                      (make-coord 515  917 -361)
                                      (make-coord -336 658 858)
                                      (make-coord -476 619 847)
                                      (make-coord -460 603 -452)
                                      (make-coord 729  430 532)
                                      (make-coord -322 571 750)
                                      (make-coord -355 545 -477)
                                      (make-coord 413  935 -424)
                                      (make-coord -391 539 -444)
                                      (make-coord 553  889 -390))
                               coord<?))

(define overlap-4-and-1-from0  (sort
                                (list
                                   (make-coord  459   -707  401)
                                   (make-coord -739  -1745  668)
                                   (make-coord -485   -357  347)
                                   (make-coord  432  -2009  850)
                                   (make-coord  528   -643  409)
                                   (make-coord  423   -701  434)
                                   (make-coord -345   -311  381)
                                   (make-coord  408  -1815  803)
                                   (make-coord  534  -1912  768)
                                   (make-coord -687  -1600  576)
                                   (make-coord -447   -329  318)
                                   (make-coord -635  -1737  486))
                                coord<?))

(define allpairs-1-and-0 (zip-alists (scan-pairs (first example-scans)) (scan-pairs (second example-scans))))

(test-equal overlap-0-and-1-from0
            (sort (delete-duplicates (append (map caadr allpairs-1-and-0) (map cdadr allpairs-1-and-0))) coord<?))

(test-equal overlap-0-and-1-from1
            (sort (delete-duplicates (append (map caaddr allpairs-1-and-0) (map cdaddr allpairs-1-and-0))) coord<?))

(define allpairs-4-and-1 (zip-alists (scan-pairs (second example-scans)) (scan-pairs (fifth example-scans))))

(test-equal (make-coord 68 -1246 -43)
            (let ((transform (car
                              (scan-give-transform (second example-scans) (first example-scans)))))
              (transform (make-coord 0 0 0))))
(dbg "XXXX" "XXXX")

(define transform-1->0 (car (scan-give-transform (second example-scans) (first example-scans))))
(define transform-4->1 (car (scan-give-transform (fifth example-scans) (second example-scans))))

(test-equal overlap-4-and-1-from0
            (sort
             (map transform-1->0 (delete-duplicates (append (map caadr allpairs-4-and-1) (map cdadr allpairs-4-and-1))))
             coord<?))

(display transform-4->1)
(transform-4->1 (make-coord 0 0 0))
(test-equal (make-coord -20 -1133 1061)
            (transform-1->0 (transform-4->1 (make-coord 0 0 0))))

(test-end "XXX")

