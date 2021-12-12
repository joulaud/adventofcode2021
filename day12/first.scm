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
                  (graph-add-edge graph (car pair) (cdr pair)))
               graph
               x)))
     x))

(define-record-type <node>
    (make-node name neighbours visit-status)
    node?
    (name node-name) ; string identifying the node
    (neighbours node-neighbours) ; names of adjacent nodes
    (visit-status node-visit-status)) ; symbol among 'untouched 'ongoing 'done
(define (make-empty-node name)
   (make-node name '() 'untouched))
(define (node-add-neighbour node neighbour-name)
   (let* ((name (node-name node))
          (neighbours (node-neighbours node))
          (visit-status (node-visit-status node)))
     (make-node name (cons neighbour-name neighbours) visit-status)))


(define-record-type <graph>
   ;; A <graph> is an association list from node names to <node>s
   (make-graph nodes)
   graph?
   (nodes graph-nodes))
(define (make-empty-graph)
  (make-graph '()))

(define (graph-add-edge graph name1 name2)
   (let* ((nodes (graph-nodes graph))
          (node1 (assoc name1 nodes))
          (node2 (assoc name2 nodes))
          (node1 (if node1 (cdr node1) (make-empty-node name1)))
          (node2 (if node2 (cdr node2) (make-empty-node name2)))
          (node1 (node-add-neighbour node1 name2))
          (node2 (node-add-neighbour node2 name1))
          (nodes (assoc-set! nodes name1 node1))
          (nodes (assoc-set! nodes name2 node2)))
     (make-graph nodes)))

(define (graph-ref graph name)
   (let* ((nodes (graph-nodes graph))
          (node (assoc name nodes))
          (node (if node (cdr node) (make-empty-node name))))
      node))


(define-public (main args)
  (let* (
         (result1 "UNIMP")
         (result2 "UNIMP"))
    (format #t "result1: ~a\n" result1)
    (format #t "result2: ~a\n" result2)))
