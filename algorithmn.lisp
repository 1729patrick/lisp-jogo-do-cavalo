(let ((player -1))
  (defun jogar (board time)
    (format t "~a" (negamax board player time 3 most-negative-fixnum most-positive-fixnum 1))

    (setf player (opposite player))
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


(let (
  (best-value most-negative-fixnum);;bestValue := −∞
  )


(defun negamax (board node time depth α β cor)
  (format t "~a" (generate-moves board node))
  (cond
    ((= depth 0) (* node cor));;se d = 0 ou n é terminal ;;return c * valor heuristico de n

    (t (let* (
      (successors (generate-moves board node));;sucessores := OrderMoves(GenerateMoves(n))
      )

      (successors-loop successors board node time depth α β cor)

     best-value
    )
   )
  )
)

(successors-loop (successors board node time depth α β cor) ;;para cada sucessor nk em sucessores
(cond ((null successors) nil)
      (t
          (setf best-value (max best-value (- (negamax board successor time (- depth 1) (- α) (- β) (- cor))))) ;; bestValue := max (bestValue, −negamax (nk, d−1, −β, − α, −c))
          (setf α (max α best-value)) ;; α := max (α, bestValue)
          (successors-loop (cdr successors) board node time depth α β cor)
      )
      )
)

(defun opposite (player)
(cond
      ((equal player -1) -2)
      (t -1)
    )
)
