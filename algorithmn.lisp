(let ((player -1))
  (defun jogar (board time)
    (negamax board time player)

    (setf player (opposite player))
  )
)

(defun negamax (board time player)
    (setq bestValue -10)
             (setq bestPath nil)
             (setq successors (generate-moves board player))
             (loop for successor in successors
                do
                   (let* ((result (negamax successor (opposite player)))
                         (value (- (first result))))
                         (when (> value bestValue)
                              (setq bestValue value)
                              (setq bestPath successor))))
             (cons bestValue bestPath)
      )



(defun opposite (player)
(cond
      ((equal player -1) -2)
      (t -1)
    )
)
