(use-modules (ice-9 match))
(match-let* (((x y) (list 1 2))
             (a (list x 4)))
  (list a x y))

