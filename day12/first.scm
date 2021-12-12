(define-module (first)
    #:use-module (ice-9 rdelim)
    #:use-module (ice-9 match)
    #:use-module (ice-9 format)
    #:use-module (srfi srfi-1) ; lists library
    #:use-module (srfi srfi-2) ; and-let*
    #:use-module (srfi srfi-11) ; let*-values
    #:use-module (srfi srfi-9)
    #:use-module (srfi srfi-9 gnu) ; immutable records
    #:use-module (srfi srfi-41) ; Streams
    #:use-module (srfi srfi-43)) ; Vectors iterators

(use-modules (ice-9 rdelim))
(use-modules (ice-9 match))
(use-modules (ice-9 format))
(use-modules (srfi srfi-1)) ; lists library
(use-modules (srfi srfi-11)) ; let*-values
(use-modules (srfi srfi-9))
(use-modules (srfi srfi-9 gnu)) ; immutable records
(use-modules (srfi srfi-41))

(define (dbg t v) (format #t "~s: ~a\n" t v) (force-output))

(define stream-of-lines
  (stream-lambda (port)
     (let
         ((line (read-line port)))
      (cond
       ((eof-object? line) stream-null)
       (else (stream-cons line (stream-of-lines port)))))))

(define (array-for-each-index a proc-cell proc-endofline)
  (let* ((dims (array-dimensions a))
         (maxl (car dims))
         (maxc (cadr dims)))
    (do ((i 0 (1+ i)))
        ((>= i maxl))
      (do ((j 0 (1+ j)))
          ((>= j maxc))
        (proc-cell i j (array-ref a i j)))
      (proc-endofline i))))



(define (print-array a)
  (array-for-each-index
       a
       (lambda (i j v) (if v (display "X") (display ".")))
       (lambda (i) (display "\n"))))

(define (line->pair line)
   (let* (
          (x (string-split line #\-))
          (x (cons (car x) (cadr x))))
     x))

(define (stream-of-lines->graph strm)
   (let* ((x (stream-map line->pair  strm))
          (graph (make-empty-graph))
          (x (stream-fold
               (lambda (graph pair)
                  (graph-add-edge! graph (car pair) (cdr pair)))
               graph
               x)))
     x))

(define-record-type <node>
    (make-node name neighbours visit-status big?)
    node?
    (name node-name) ; string identifying the node
    (neighbours node-neighbours) ; names of adjacent nodes
    (visit-status node-visit-status) ; number of times we visited cave
    (big? node-big?))
(define (make-empty-node name)
 (let* ((c (string-ref name 0))
        (big? (char-upper-case? c)))
   (make-node name '() 0 big?)))
(define (node-add-neighbour node neighbour-name)
   (let* ((name (node-name node))
          (neighbours (node-neighbours node))
          (big? (node-big? node))
          (visit-status (node-visit-status node)))
     (make-node name (cons neighbour-name neighbours) visit-status big?)))

(define-record-type <graph>
   ;; A <graph> is an association list from node names to <node>s
   (make-graph nodes edges visited)
   graph?
   (nodes graph-nodes)
   (edges graph-edges)
   (visited graph-visited))
(define (make-empty-graph)
  (make-graph '() '() '()))

(define (edges-add-edge! edges from to)
   (let* ((neighbours (assoc from edges))
          (neighbours (if neighbours (cons to (cdr neighbours)) (list to)))
          (edges (assoc-set! edges from neighbours)))
      edges))

(define (graph-add-edge! graph node1 node2)
   ;; Note: visited list of nodes is reset to empty when adding edge
   ;; Note: edges can be modified destructively in graph
   (let* ((nodes (graph-nodes graph))
          (nodes (if (member node1 nodes) nodes (cons node1 nodes)))
          (nodes (if (member node2 nodes) nodes (cons node2 nodes)))
          (edges (graph-edges graph))
          (edges (edges-add-edge! edges node1 node2))
          (edges (edges-add-edge! edges node2 node1)))
     (make-graph nodes edges '())))

(define (graph-neighbours graph from)
   (let* ((edges (graph-edges graph))
          (neighbours (assoc from edges))
          (neighbours (or (cdr neighbours) '())))
      neighbours))

(or #f '())
(define-record-type <path>
  (make-path lst)
  path?
  (lst path->list))

(define (path-cons node-name path)
  (make-path (cons node-name (path->list path))))

(define (edges+visited->paths edges visited from no-twice?)
  (cond
      ((string= from "end")
       (list (make-path '("end"))))
      ((and (string= from "start")
            (assoc from visited))
       '())
      ((and
         no-twice?
         (char-lower-case? (string-ref from 0))
         (assoc from visited))
       ;; already visited small node, no path
       '())
      (else
       (let* ((no-twice? (or no-twice?
                             (and (char-lower-case? (string-ref from 0))
                                  (assoc from visited))))
              (new-visited (acons from #t visited))
              (neighbours (assoc from edges))
              (neighbours (if neighbours (cdr neighbours) '()))
              (subpaths (append-map
                           (lambda (next-node)
                               (edges+visited->paths edges new-visited next-node
                                                     no-twice?))
                           neighbours))
              (paths (map
                        (lambda (subpath) (path-cons from subpath))
                        subpaths)))
         paths))))

(define (graph->paths graph from no-twice?)
   (edges+visited->paths (graph-edges graph) (graph-visited graph) from no-twice?))

(define-public (main args)
  (let* (
         (strm (stream-of-lines (current-input-port)))
         (graph (stream-of-lines->graph strm))
         (result1 (length (graph->paths graph "start" #t)))
         (result2 (length (graph->paths graph "start" #f))))
    (format #t "result1: ~a\n" result1)
    (format #t "result2: ~a\n" result2)))
