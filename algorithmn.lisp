(let (
  (*player nil)
  (*board (bo))
  (*points-1 0)
  (*points-2 0)
)
  (defun jogar (board time)
    (let* (
      (move (negamax board *player time 1 most-negative-fixnum most-positive-fixnum 1))
      (indexes-move (position-node move board))
      (indexes-player (position-node *player board))
      (board-with-updated-last-move (replace-value (first indexes-player) (second indexes-player) board nil))
      (board-with-updated-player-position (replace-value (first indexes-move) (second indexes-move) board-with-updated-last-move *player))
      )

    (cond
      ((not (null board-with-updated-player-position))
        (display-computer-move *player (position-indexes-to-chess indexes-move) board-with-updated-player-position)

        (sum-points move)

        board-with-updated-player-position

      )
      (t *board)
      )
      )
  )


;;(human-play "I3")
(defun human-play (position)
    (let* (
      (move (car (value-node (first (position-chess-to-indexes (string position))) (second (position-chess-to-indexes (string position))) *board)))
      (indexes-move (position-node move *board))
      (indexes-player (position-node *player *board))
      (board-with-updated-last-move (replace-value (first indexes-player) (second indexes-player) *board nil))
      (board-with-updated-player-position (replace-value (first indexes-move) (second indexes-move) board-with-updated-last-move *player))
      )

      board-with-updated-player-position
      )
  )


  (defun game-cc (time)
    ;;resetar as váriaveis apos cada partida
    (setf *board (bo))
    (setf *points-1 0)
    (setf *points-2 0)

    (loop while (or (not (null (generate-moves *board *player))) (not (null (generate-moves *board (opposite *player)))))
      do

      (setf *board (jogar *board time))
      (setf *player (opposite *player))
    )

    (display-points-players *points-1 *points-2)
  )

  ;;human = -1
  ;;computer = -2

  (defun game-hc (time &optional (first-player -1))
    (setf *player first-player)
    ;;resetar as váriaveis apos cada partida
    (setf *board (bo))
    (setf *points-1 0)
    (setf *points-2 0)

    (loop while (or (not (null (generate-moves *board *player))) (not (null (generate-moves *board (opposite *player)))))
      do

      (cond
        ((equal *player -1)
            (let* (
              (moves (generate-moves *board *player))
              (moves-avaliable (mapcar
                                  (lambda (value)
                                      (position-indexes-to-chess (position-node value *board)))
                                moves)
              )
            )


            (setf *board (human-play (read-play moves-avaliable)))
          )

          (terpri)
          (display-board *board)
        )
        (t (setf *board (jogar *board time)))
      )

      (setf *player (opposite *player))
    )

    (display-points-players *points-1 *points-2)
  )

  (defun sum-points (move)
    (cond
      ((eq *player -1) (setf *points-1 (+ *points-1 move)))
      (t (setf *points-2 (+ *points-2 move)))
    )
  )
)

;;; argumentos: nó n, profundidade d, cor c
;;; b = ramificação (número de sucessores)
;; function negamax (n, d, α, β, c)
;; se d = 0 ou n é terminal OK
;; return c * valor heuristico de n OK
;; sucessores := OrderMoves(GenerateMoves(n)) OK
;; bestValue := −∞ OK
;; para cada sucessor nk em sucessores ok
;; bestValue := max (bestValue, −negamax (nk, d−1, −β, − α, −c)) OK

;; α := max (α, bestValue)
;; se α ≥ β
;; break
;; return bestValue OK

(defun negamax (board node time depth α β cor)
  (cond
    ((= depth 0) (* node cor));;se d = 0 ou n é terminal ;;return c * valor heuristico de n

    (t (let* (
      (successors (generate-moves board node));;sucessores := OrderMoves(GenerateMoves(n))
      )

      (successors-loop successors board node time depth α β cor)
    )
   )
  )
)

(defun successors-loop (successors board node time depth α β cor &optional (best-value most-negative-fixnum)) ;;para cada sucessor nk em sucessores
(cond (
      (null successors) best-value)
      (t
        (let* (
          (best-value (max best-value (- (negamax board (car successors) time (- depth 1) (- β) (- α) (- cor))))) ;; bestValue := max (bestValue, −negamax (nk, d−1, −β, − α, −c))
          (α (max α best-value)) ;; α := max (α, bestValue))
        )

        (if (>= α β)
        best-value
        (successors-loop (cdr successors) board node time depth α β cor best-value))
        )
      )
    )
)

(defun opposite (player)
(cond
      ((equal player -1) -2)
      (t -1)
    )
)
