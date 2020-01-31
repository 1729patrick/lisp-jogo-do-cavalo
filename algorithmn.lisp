;;statistics
(defvar *alpha-cuts* 0)
(defvar *beta-cuts* 0)
(defvar *analyzed-nodes* 0)

;;hashtable
;;(make-hash-table &key test size rehash-size rehash-threshold)
(defparameter *memoization-hash* (make-hash-table))
(defparameter *memoization-points-hash* (make-hash-table))


(let (
  (*player -1)
  (*board (tabuleiro-aleatorio))
  (*points-1 0)
  (*points-2 0)
  (*best-play (make-play-node (bo) most-negative-fixnum most-negative-fixnum -5))
)
  (defun jogar (board time depth)

    (setf *beta-cuts* 0)
    (setf *alpha-cuts* 0)
    (setf *analyzed-nodes* 0)
    (setf *best-play (make-play-node *board most-negative-fixnum most-negative-fixnum -5))

    (let* (
      (play-start-time (get-internal-real-time));;start-time in miliseconds
      (move (negamax (make-node -5 *points-1 *points-2 board -5 *player 0) time depth most-negative-fixnum most-positive-fixnum 1 play-start-time))
      (best-move (play-node-move *best-play))
      (indexes-move (position-node move board))
      (indexes-player (position-node *player board))
      (board-with-updated-last-move (replace-value (first indexes-player) (second indexes-player) (remove-simmetric-assimmetric move board) nil))
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

          (write-log board-with-updated-player-position *points-1 *points-2 (get-play-time-milisecs play-start-time) *alpha-cuts* *beta-cuts* *analyzed-nodes*)
          board-with-updated-player-position
        )
        (t  
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
      (board-with-updated-last-move (replace-value (first indexes-player) (second indexes-player) (remove-simmetric-assimmetric move *board) nil))
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


  (defun game-cc (time depth)
    (reset-variables)

    (display-start-board *board)
    (loop while (or (not (null (generate-moves (make-node -5 *points-1 *points-2 *board -5 *player 0)))) 
      (not (null (generate-moves (make-node -5 *points-1 *points-2 *board -5 (opposite *player) 0)))) ;;se pelo menos 1 player n pode jogar
        (equal 100 (length (remove-nil-value *board))))
      do

      (setf *board (jogar *board time depth))
      (setf *player (opposite *player))
    )

    (display-points-players *points-1 *points-2)
  )

  ;;human = -1
  ;;computer = -2
  (defun game-hc (time depth &optional (first-player -1))
    (reset-variables)

    (setf *player first-player)
    (display-start-board *board)

    (loop while (or (not (null (generate-moves (make-node -5 *points-1 *points-2 *board -5 *player 0)))) 
        (not (null (generate-moves (make-node -5 *points-1 *points-2 *board -5 (opposite *player) 0)))) 
          (equal 100 (length (remove-nil-value *board))))
      do

      (cond
        ((equal *player -1)
            (let* (
              (moves (list-points (generate-moves (make-node -5 *points-1 *points-2 *board -5 *player 0))))
              (moves-avaliable (values-to-chess moves *board))
            )

            (cond
            ((null moves-avaliable) 
            ;;adicionar tempo da jogada do humano?????
              (setf *board (human-play (read-play (values-to-chess (remove-point-list-nil (car *board)) *board)))))
            (t (setf *board (human-play (read-play moves-avaliable))))
            )
          )

          (terpri)
          (display-board *board)
        )
        (t (setf *board (jogar *board time depth)))
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
    (setf *board (tabuleiro-aleatorio))
    (setf *player -1)
    (setf *points-1 0)
    (setf *points-2 0)
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
;;;node = player
(defun negamax (node time-limit depth α β cor &optional (play-start-time (get-internal-real-time)))
    
  (cond
      ;;se d = 0 ou n é terminal ou tempo terminou ;;return c * valor heuristico de n
      ((OR (= depth 0) 
        (>= (- (get-internal-real-time) play-start-time) time-limit)
          (null (generate-moves node))) 
            (* (node-points node) cor))

      (t (let* (
          (successors (sort-f-nodes (generate-moves node)));;sucessores := OrderMoves(GenerateMoves(n))
        )
      ;;loop de sucessores
      (successors-loop successors (node-board node) (node-player node) time-limit depth α β cor play-start-time)
    )
   )
  )
)

(defun successors-loop (successors board player time depth α β cor play-start-time &optional (l-best-value most-negative-fixnum)) ;;para cada sucessor nk em sucessores
  (cond (
        (null successors) l-best-value)
        (t

        ;;verificar se hashtable contem o valor de um estado e devolver caso sim
        (setf *analyzed-nodes* (1+ *analyzed-nodes*))
        (cond ((not (null (gethash (list board player) *memoization-points-hash*))) 
          (gethash (list board player) *memoization-points-hash*)))

          (let* (
            (best-value (max l-best-value (- (negamax (car successors) time (- depth 1) (- β) (- α) (- cor) play-start-time)))) ;; bestValue := max (bestValue, −negamax (nk, d−1, −β, − α, −c))
            (α (max α best-value)) ;; α := max (α, bestValue))
          )
         
         (cond 
                  ((and (= depth 1) (> best-value l-best-value) (< α β))
                    (setf *best-play (make-play-node board (node-f(car successors))
                     (car (value-node (first (position-node player board)) (second (position-node player board)) board))
                      player))
                      
              ))
        
          (cond 
           ( (< α β)
            (setf (gethash (list board player) *memoization-points-hash*) best-value)     
              (successors-loop (cdr successors) board player time depth α β cor play-start-time best-value))
           (t 
               (cond ((= cor 1) (setf *alpha-cuts* (1+ *alpha-cuts*))))
               (cond ((= cor -1) (setf *beta-cuts* (1+ *beta-cuts*))))
             l-best-value)
          )
          )
        )
      )
)

(defun generate-moves (node)
(let* (
      (line-column (position-node (node-player node) (node-board node)))
      (line-index (first line-column))
      (column-index (second line-column))
      (board-no-player (replace-value line-index column-index (node-board node) nil))
      )

      (cond ((null board-no-player) '())
      (t (append
        (move-avaliable (- line-index 2) (- column-index 1) board-no-player (node-player node) (node-points-p1 node) (node-points-p2 node)  (node-depth node))
        (move-avaliable (- line-index 2) (+ column-index 1) board-no-player (node-player node) (node-points-p1 node) (node-points-p2 node)  (node-depth node))
        (move-avaliable (+ line-index 2) (- column-index 1) board-no-player (node-player node) (node-points-p1 node) (node-points-p2 node)  (node-depth node)) 
        (move-avaliable (+ line-index 2) (+ column-index 1) board-no-player (node-player node) (node-points-p1 node) (node-points-p2 node)  (node-depth node))
        (move-avaliable (- line-index 1) (- column-index 2) board-no-player (node-player node) (node-points-p1 node) (node-points-p2 node)  (node-depth node))
        (move-avaliable (- line-index 1) (+ column-index 2) board-no-player (node-player node) (node-points-p1 node) (node-points-p2 node)  (node-depth node))
        (move-avaliable (+ line-index 1) (- column-index 2) board-no-player (node-player node) (node-points-p1 node) (node-points-p2 node)  (node-depth node))
        (move-avaliable (+ line-index 1) (+ column-index 2) board-no-player (node-player node) (node-points-p1 node) (node-points-p2 node)  (node-depth node))
        ))
      )
  )
)



(defun move-avaliable (line-index column-index board player points-1 points-2 depth)
   (cond
   ((or (< line-index 0) (> line-index 9)) nil)
   ((or (< column-index 0) (> column-index 9)) nil)
   ((value-node line-index column-index board)

  ;;mudar para construtores

	(let* (
			(value (car (value-node line-index column-index board)))
			(points (sum-move player value points-1 points-2))
			)
      (cond 
      ((not (null (gethash (list board player) *memoization-hash*)))         
        (list
          (make-node value (first points) (second points) (replace-value line-index column-index board player) (gethash (list board player) *memoization-hash*) player (1+ depth))
        )
      )
        (t 
          (setf (gethash (list board player) *memoization-hash*) (evaluate-node (first points) (second points) player))

          (list
            (make-node value (first points) (second points) (replace-value line-index column-index board player) (evaluate-node (first points) (second points) player) player (1+ depth))
          )
        )
      )))
  )
)

)