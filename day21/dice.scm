(define-module (adventofcode2021 day21 dice)
    #:use-module (adventofcode2021 day21 utils)
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
    #:use-module (srfi srfi-26)) ; cut (specializing parameters, currying alternative)

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

(define (dbg t v) (format #t "~a~a\n" t v) (force-output))
(define (dbgn t v) (format #t "~a~a  #  " t v) (force-output))

(define-immutable-record-type <player>
  (make-player name position score)
  player?
  (name player-name set-player-name)
  (position player-position set-player-position)
  (score player-score set-player-score))

(define-immutable-record-type <dice>
  (make-dice roll-count)
  dice?
  (roll-count dice-roll-count set-dice-roll-count))

(define (new-dice)
  (make-dice 1))

(define (player->string player)
   (let ((str (string-append
                           "[" (player-name player) ": (" (number->string (player-position player)) ") " (number->string (player-score player)) "]")))
     str))

(define (player-move player movement)
  (let* ((position (player-position player))
         (position (+ position movement))
         (position (wrapping-to-b 10 position))
         (player (set-player-position player position))
         (player (set-player-score player (+ position (player-score player)))))
     player))

(define (player-win? player)
  (>= (player-score player) 1000))

(define (wrapping-to-b base n)
   (1+ (remainder (1- n) base)))

(define (dice-roll dice)
  (let ((rolls (dice-roll-count dice)))
    (values (wrapping-to-b 100 rolls) (set-dice-roll-count dice (1+ rolls)))))

(define (dice-roll-three dice)
  (let*-values (((a dice) (dice-roll dice))
                ((b dice) (dice-roll dice))
                ((c dice) (dice-roll dice)))
      (values (+ a b c) dice)))

(define (player-play player dice)
   (let*-values (((points dice) (dice-roll-three dice))
                 ((player) (player-move player points)))
      (values player dice)))

(define (play-until-winner p1 p2 dice)
  (let loop ((p1 p1) (p2 p2) (dice dice))
    (format #t "~a ~a ~a\n" (player->string p1) (player->string p2) (dice-roll-count dice))
    (cond
     ((player-win? p2) (values p2 p1 dice))
     (else (let*-values (((p1 dice) (player-play p1 dice)))
               (loop p2 p1 dice))))))

(define-public (main args)
   (let*-values (((p1) (read-line))
                 ((p1) (string->number p1))
                 ((p1) (make-player "P1" p1 0))
                 ((p2) (read-line))
                 ((p2) (string->number p2))
                 ((p2) (make-player "P2" p2 0))
                 ((winner loser dice) (play-until-winner p1 p2 (new-dice)))
                 ((num-plays) (1- (dice-roll-count dice)))
                 ((result1) (* (player-score loser) num-plays))
                 ((result2) "UNIMP"))
      (format #t "result1: ~a\n" result1)
      (format #t "result2: ~a\n" result2)))
