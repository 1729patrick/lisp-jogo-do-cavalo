(defvar *alpha-cuts* 0)
(defvar *beta-cuts* 0)
(defvar *analised-nodes* 0)

(let (
  (*player -1)
  (*board (bo))
  (*points-1 0)
  (*points-2 0)
)
  (defun jogar (board time)
    (let* (
      (play-start-time (get-internal-real-time));;start-time in miliseconds
      (move (negamax board *player time 1 most-negative-fixnum most-positive-fixnum 1 play-start-time))
      (indexes-move (position-node move board))
      (indexes-player (position-node *player board))
      (board-with-updated-last-move (replace-value (first indexes-player) (second indexes-player) board nil))
      (board-with-updated-player-position (replace-value (first indexes-move) (second indexes-move) board-with-updated-last-move *player))
      )

    (cond
      ((and (null (first indexes-player)) (null (second indexes-player)))
        (setf move (max-first-move-value *player *board))
        (setf indexes-move (position-node move board))
        (setf board-with-updated-player-position (replace-value (first indexes-move) (second indexes-move) *board *player))
      )
    )

    (cond
      ((not (null board-with-updated-player-position))
        (display-computer-move *player (position-indexes-to-chess indexes-move) board-with-updated-player-position)

        (sum-points move)

        (write-log board-with-updated-player-position *points-1 *points-2 (get-play-time-milisecs play-start-time) nil)
        board-with-updated-player-position
      )
      (t (write-log *board *points-1 *points-2 (get-play-time-milisecs play-start-time) nil) 
         *board)
      )
      )
  )


;;(human-play "I3")
(defun human-play (position)
    (let* (
      (move (car (value-node (first (position-chess-to-indexes position)) (second (position-chess-to-indexes position)) *board)))
      (indexes-move (position-node move *board))
      (indexes-player (position-node *player *board))
      (board-with-updated-last-move (replace-value (first indexes-player) (second indexes-player) *board nil))
      (board-with-updated-player-position (replace-value (first indexes-move) (second indexes-move) board-with-updated-last-move *player))
      )

      (cond
      ((and (null (first indexes-player)) (null (second indexes-player)))
        (setf indexes-move (position-node move *board))
        (setf board-with-updated-player-position (replace-value (first indexes-move) (second indexes-move) *board *player))
      )
    )

      (sum-points move)
      board-with-updated-player-position
      )
  )


  (defun game-cc (time)
    (reset-variables)

    (display-start-board *board)
    (let ()  )
    (loop while (or (not (null (generate-moves *board *player))) (not (null (generate-moves *board (opposite *player)))) (equal 100 (length (remove-nil-value *board))))
      do

      (setf *board (jogar *board time))
      (setf *player (opposite *player))
    )

    (display-points-players *points-1 *points-2)
  )

  ;;human = -1
  ;;computer = -2
  (defun game-hc (time &optional (first-player -1))
    (reset-variables)

    (setf *player first-player)
    (display-start-board *board)

    (loop while (or (not (null (generate-moves *board *player))) (not (null (generate-moves *board (opposite *player)))) (equal 100 (length (remove-nil-value *board))))
      do

      (cond
        ((equal *player -1)
            (let* (
              (moves (generate-moves *board *player))
              (moves-avaliable (values-to-chess moves *board))
            )

            (cond
            ((null moves-avaliable) 
            ;;adicionar tempo da jogada do humano?????
              (setf *board (human-play (read-play (values-to-chess (car *board) *board)))))
            (t (setf *board (human-play (read-play moves-avaliable))))
            )
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

   ;;resetar as váriaveis apos cada partida
  (defun reset-variables ()
    (setf *board (bo))
    (setf *player -1)
    (setf *points-1 0)
    (setf *points-2 0)
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

(defun negamax (board node time-limit depth α β cor &optional (play-start-time (get-internal-real-time)))
  (cond
      ;;((>= (- (get-internal-real-time) play-start-time) time-limit) nil)
      ((or (= depth 0) (>= (- (get-internal-real-time) play-start-time) time-limit))  (* node cor));;se d = 0 ou n é terminal ;;return c * valor heuristico de n

    (t (let* (
      (successors (generate-moves board node));;sucessores := OrderMoves(GenerateMoves(n))
      )

      (successors-loop successors board node time-limit depth α β cor play-start-time)
    )
   )
  )
)

(defun successors-loop (successors board node time depth α β cor play-start-time &optional (best-value most-negative-fixnum)) ;;para cada sucessor nk em sucessores
(cond (
      (null successors) best-value)
      (t
        (let* (
          (best-value (max best-value (- (negamax board (car successors) time (- depth 1) (- β) (- α) (- cor) play-start-time)))) ;; bestValue := max (bestValue, −negamax (nk, d−1, −β, − α, −c))
          (α (max α best-value)) ;; α := max (α, bestValue))
        )

        (if (>= α β)
        best-value
        (successors-loop (cdr successors) board node time depth α β cor play-start-time best-value))
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
