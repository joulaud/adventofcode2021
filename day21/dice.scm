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

(define-immutable-record-type <game>
  (make-game score-p1 position-p1 score-p2 position-p2)
  game?
  (score-p1 score-p1 set-score-p1)
  (position-p1 position-p1 set-position-p1)
  (score-p2 score-p2 set-score-p2)
  (position-p2 position-p2 set-position-p2))

(define-public (make-all-possible-games)
  (let* ((scores (iota 22 0))
         (positions (iota 10 1))
         (combinations (combine*
                          scores
                          positions
                          scores
                          positions)))
   (map (cut apply make-game <>) combinations)))

(define initial-games-with-all-possible
    (fold (cut vhash-cons <> 0 <>)
          vlist-null
          (make-all-possible-games)))

(define (initial-games pos-p1 pos-p2)
  (let ((game (make-game pos-p1 0 pos-p2 0)))
    (vhash-cons game 1 vlist-null)))

(define (game-winner? game)
  (or (>= (score-p1 game) 21)
      (>= (score-p2 game) 21)))

(define (all-winners? games)
  (let ((count-not-winner (vhash-fold
                            (lambda (g _ res)
                              (+ res (if (game-winner? g) 0 1)))
                            0
                            games)))
    (dbg "count-not-winner=" count-not-winner)
    (<= count-not-winner 0)))

(define (play-player1 game movement)
  (cond
   ((game-winner? game)
    game)
   (else
       (let* ((position (position-p1 game))
              (position (+ position movement))
              (position (wrapping-to-b 10 position))
              (new-score (+ position (score-p1 game)))
              (game (set-position-p1 game position))
              (game (set-score-p1 game new-score)))
          game))))

(define (play-player2 game movement)
  (cond
   ((game-winner? game)
    game)
   (else
       (let* ((position (position-p2 game))
              (position (+ position movement))
              (position (wrapping-to-b 10 position))
              (new-score (+ position (score-p2 game)))
              (game (set-position-p2 game position))
              (game (set-score-p2 game new-score)))
          game))))

(define (inc-game-in-vlist game num vlist)
  (let* ((v (vhash-assoc game vlist))
         (vlist (vhash-delete game vlist))
         (new (if v (+ (cdr v) num) num))
         (vlist (vhash-cons game new vlist)))
     vlist))

(define (play-1-game-player1 game num vlist)
  (let* ((new-game-dice-1 (play-player1 game 1))
         (vlist (inc-game-in-vlist new-game-dice-1 num vlist))
         (new-game-dice-2 (play-player1 game 2))
         (vlist (inc-game-in-vlist new-game-dice-2 num vlist))
         (new-game-dice-3 (play-player1 game 3))
         (vlist (inc-game-in-vlist new-game-dice-3 num vlist)))
    vlist))

(define (play-1-game-player2 game num vlist)
  (let* ((new-game-dice-1 (play-player2 game 1))
         (vlist (inc-game-in-vlist new-game-dice-1 num vlist))
         (new-game-dice-2 (play-player2 game 2))
         (vlist (inc-game-in-vlist new-game-dice-2 num vlist))
         (new-game-dice-3 (play-player2 game 3))
         (vlist (inc-game-in-vlist new-game-dice-3 num vlist)))
    vlist))

(define (play-1-dice-player1 games)
  (vhash-fold play-1-game-player1 vlist-null games))

(define (play-1-dice-player2 games)
  (vhash-fold play-1-game-player2 vlist-null games))

(define (play-all games)
     (cond
      ((all-winners? games)
       (let* ((wins-player1 (vhash-fold
                             (lambda (g n count) (if (>= (score-p1 g) 21) (+ n count) count))
                             0
                             games))
              (wins-player2 (vhash-fold
                             (lambda (g n count) (if (>= (score-p2 g) 21) (+ n count) count))
                             0
                             games)))
         (values wins-player1 wins-player2)))
      (else
       (let* ((new-games vlist-null)
              (new-games (vhash-fold play-1-game-player1 new-games games))
              (new-games (vhash-fold play-1-game-player1 new-games games))
              (new-games (vhash-fold play-1-game-player1 new-games games))
              (new-games (vhash-fold play-1-game-player2 new-games games))
              (new-games (vhash-fold play-1-game-player2 new-games games))
              (new-games (vhash-fold play-1-game-player2 new-games games)))
         (play-all new-games)))))

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
                 ((player1) (make-player "P1" p1 0))
                 ((p2) (read-line))
                 ((p2) (string->number p2))
                 ((player2) (make-player "P2" p2 0))
                 ((winner loser dice) (play-until-winner player1 player2 (new-dice)))
                 ((num-plays) (1- (dice-roll-count dice)))
                 ((result1) (* (player-score loser) num-plays))
                 ((first-game) (make-game 0 p1 0 p2))
                 ((first-games) (vhash-cons first-game 1 vlist-null))
                 ((wins-p1 wins-p2) (play-all first-games)))
      (format #t "result1: ~a\n" result1)
      (format #t "result2 wins player1: ~a\n" wins-p1)
      (format #t "result2 wins player2: ~a\n" wins-p2)))
