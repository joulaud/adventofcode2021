(define-module (adventofcode2021 day22 reactor)
    #:use-module (adventofcode2021 day22 utils)
    #:use-module (ice-9 rdelim)
    #:use-module (ice-9 match)
    #:use-module (ice-9 format)
    #:use-module (srfi srfi-1) ; lists library
    #:use-module (srfi srfi-2) ; and-let*
    #:use-module (srfi srfi-11) ; let*-values
    #:use-module (srfi srfi-9)
    #:use-module (srfi srfi-9 gnu) ; immutable records
    #:use-module (srfi srfi-41) ; Streams
    #:use-module (srfi srfi-43) ; Vectors iterators
    #:use-module (srfi srfi-26) ; cut (specializing parameters, currying alternative)
    #:use-module (srfi srfi-69)) ; hash tables

(use-modules (ice-9 rdelim))
(use-modules (ice-9 match))
(use-modules (ice-9 format))
(use-modules (ice-9 vlist))
(use-modules (srfi srfi-2)) ; and-let*
(use-modules (srfi srfi-1)) ; lists library
(use-modules (srfi srfi-11)) ; let*-values
(use-modules (srfi srfi-9))
(use-modules (srfi srfi-9 gnu)) ; immutable records
(use-modules (srfi srfi-41))
(use-modules (srfi srfi-26)) ; cut (specializing parameters, currying alternative)
(use-modules (srfi srfi-69)) ; hash tables

(define (dbg t v) (format #t "~a~a\n" t v) (force-output))
(define (dbgn t v) (format #t "~a~a  #  " t v) (force-output))

(define-immutable-record-type <cuboid>
  (make-cuboid x-min x-max y-min y-max z-min z-max on?)
  cuboid?
  (x-min cuboid-x-min set-cuboid-x-min)
  (x-max cuboid-x-max set-cuboid-x-max)
  (y-min cuboid-y-min set-cuboid-y-min)
  (y-max cuboid-y-max set-cuboid-y-max)
  (z-min cuboid-z-min set-cuboid-z-min)
  (z-max cuboid-z-max set-cuboid-z-max)
  (on? cuboid-on? set-cuboid-on))

(define-record-type <reactor>
 (make-reactor-internal state cuboid)
 reactor?
 (state reactor-get-state! set-reactor-state!)
 (cuboid reactor-cuboid))

(define cuboid-50
    (make-cuboid -50 50
                 -50 50
                 -50 50
                 #f))

(define (make-reactor cuboid)
  (make-reactor-internal (make-hash-table)
                         cuboid-50))

(define (coord-list->cuboid lst)
  (make-cuboid (first  (first lst))
               (second (first lst))
               (first  (second lst))
               (second (second lst))
               (first  (third lst))
               (second (third lst))
               #t))

(define (parse-reboot-step line)
  (error "UNIMP"))

(use-modules (statprof))
(define-public (main args)
   (let*-values (
                 ((result1) "UNIMP")
                 ((result2) "UNIMP"))
      (format #t "result1: ~a\n" result1)
      (format #t "result2: ~a\n" result2)))
