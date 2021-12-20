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
(test-equal (make-scan (list (make-coord -1 -1 1) (make-coord -2 -2 2)))
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
