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

(define-immutable-record-type <game>
  (make-game score-p1 position-p1 score-p2 position-p2)
  game?
  (score-p1 score-p1 set-score-p1)
  (position-p1 position-p1 set-position-p1)
  (score-p2 score-p2 set-score-p2)
  (position-p2 position-p2 set-position-p2))

(define (game->string game)
  (format #f "(~a)~a  . (~a)~a"
          (position-p1 game) (score-p1 game)
          (position-p2 game) (score-p2 game)))

(define win-score 1000)

(define (game-winner? game)
  (or (>= (score-p1 game) win-score)
      (>= (score-p2 game) win-score)))

(define (add-counts-win-not-win-win-p1-win-p2 g n res)
  (let* ((win (first res))
         (win (if (game-winner? g) (+ win n) win))
         (not-win (second res))
         (not-win (if (game-winner? g) not-win (+ not-win n)))
         (p1 (third res))
         (p1 (if (>= (score-p1 g) win-score) (+ p1 n) p1))
         (p2 (fourth res))
         (p2 (if (>= (score-p2 g) win-score) (+ p2 n) p2)))
    (list win not-win p1 p2)))

(define (counting games)
  (hash-table-fold
                 games
                 add-counts-win-not-win-win-p1-win-p2
                 (list 0 0 0 0)))

(define (all-winners? games)
  (let ((count-not-winner (hash-table-fold
                            games
                            (lambda (g n res)
                              (+ res (if (game-winner? g) 0 n)))
                            0))
        (count-winner (hash-table-fold
                          games
                          (lambda (g n res)
                            (+ res (if (not (game-winner? g)) 0 n)))
                          0)))

    (<= count-not-winner 0)))

(define (play-player1 game movement)
  ;(dbg "p1 winner,move=" (list (game-winner? game) movement))
  ;(dbg "before=" (game->string game))
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
         ;(dbg " after=" (game->string game))
         game))))

(define (play-player2 game movement)
  ;(dbg "p2 winner,move=" (list (game-winner? game) movement))
  ;(dbg "before=" (game->string game))
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
          ;(dbg " after=" (game->string game))
          game))))

(define (inc-game-in-vlist! game num vlist)
  (let* ((v (hash-table-ref/default vlist game 0))
         (new (+ v num))
         (_ (hash-table-set! vlist game new)))
     vlist))

(define dice-results
  (let* ((dice-combinations (combine* (iota 3 1) (iota 3 1) (iota 3 1)))
         (dice-results (map (cut apply + <>) dice-combinations)))
   dice-results))

(define (new-dice)
  (make-dice 1))

(define my-dice (new-dice))
(define (my-dice-results)
  (let*-values (((val dice) (dice-roll-three my-dice)))
      (set! my-dice dice)
      (list val val)))

(define (play-1-game-func f)
 (lambda (game num new-games)
  (cond
   ((game-winner? game)
    (hash-table-set! new-games game num)
    new-games)
   (else
     (let* (
            (games-after-quantum-dice (map (cut f game <>) (my-dice-results)))
            (new-games (fold
                        (cut inc-game-in-vlist! <> num <>)
                        new-games
                        games-after-quantum-dice)))
       new-games)))))

(define play-1-game-player1
    (play-1-game-func play-player1))

(define play-1-game-player2
    (play-1-game-func play-player2))

(define (play-all games)
     (cond
      ((all-winners? games)
       (values 0 0))
      (else
       (let* (;(_ (dbg "G1=" games))
              (_ (dbg "game0=" (hash-table-fold games (lambda (k v acc) (cons (game->string k) acc)) '())))
              (games (hash-table-fold games play-1-game-player1 (make-hash-table)))
              (_ (dbg "count1=" (counting games)))
              (_ (dbg "game1=" (hash-table-fold games (lambda (k v acc) (cons (game->string k) acc)) '())))
              (games (hash-table-fold games play-1-game-player2 (make-hash-table)))
              (_ (dbg "count2=" (counting games)))
              (_ (dbg "game2=" (hash-table-fold games (lambda (k v acc) (cons (game->string k) acc)) '()))))
         (play-all games)))))

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

(define (make-new-games first-game)
 (let* ((games (make-hash-table))
        (_ (hash-table-set! games first-game 1)))
   games))

(use-modules (statprof))
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
                 ((first-games) (make-new-games first-game))
                 (x (play-all (make-new-games (make-game 0 4 0 8))))
                 (_ (dbg "x=" x))
                 ((wins-p1 wins-p2) (values 42 42))) ;(play-all first-games)))
      (format #t "result1: ~a\n" result1)
      (format #t "dices=~a\n" dice-results)
      (format #t "two-dices=~a\n" (length (combine* dice-results dice-results)))
      (format #t "result2 wins player1: ~a\n" wins-p1)
      (format #t "result2 wins player2: ~a\n" wins-p2)))
