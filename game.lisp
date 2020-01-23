;;board-order
(defun bo()
  '(
    (00 01 02 03 04 -1 06 07 08 09)
    (10 11 12 13 14 15 16 17 18 19)
    (20 21 22 23 24 25 26 27 28 29)
    (30 31 32 33 34 35 36 37 38 39)
    (40 41 42 43 44 45 46 47 48 49)
    (50 51 52 53 54 55 56 57 58 59)
    (60 61 62 63 64 65 66 67 68 69)
    (70 71 72 73 74 75 76 77 78 79)
    (80 81 82 83 84 85 86 87 88 89)
    (90 91 92 93 94 95 -2 97 98 99)
    )
)

;;<node>= current-points, points-of-player1, points-of-player2, <state>
;;;<state>= current-board
(defun make-node(points p-p1 p-p2 board)
  (list points p-p1 p-p2 board)
)

(defun node-points(node)
  (first node)
)

(defun node-points-p1(node)
  (second node)
)

(defun node-points-p2(node)
  (third node)
)

(defun node-board(node)
  (fourth node)
)

(defun test-node ()
  (car (generate-moves (bo) -2 10 10))
)

;;Node value heuristic
(defun evaluate-node(node)
 "returns the value of the node between -1 and 1, knowing that the total points in the board are 4950"
  (let ((point-variation (- (node-points-p1 node) (node-points-p2 node))))
    (cond
      ((> point-variation 2475) 1)
      ((< point-variation -2475) -1)
      ((equal point-variation 0) 0)
      (t (float (/ point-variation 4950)))
    ))
)

(defun best-play-value(*best-play)
  (first *best-play)
)

(defun best-play-points(*best-play)
  (second *best-play)
)

(defun best-play-board(*best-play)
  (third *best-play)
)

;;bestplay= heuristic-value position-value board
(defun set-best-value(*best-play value)
  (list value (best-play-points *best-play) (best-play-board *best-play))
)


(defun set-best-points(*best-play points)
  (list (best-play-value *best-play) points (best-play-board *best-play))
)

(defun set-best-board(*best-play board)
  (list (best-play-value *best-play) (best-play-points *best-play) board)
)


;;(position-chess-to-indexes "I3")
(defun position-chess-to-indexes (position)
"returns a positition converted into a line and column indexes"
	(list (- (char-code (character (string-upcase (subseq position 0 1)))) 65) (1- (parse-integer (remove (character (subseq position 0 1)) position))))
)

;;(position-indexes-to-chess (position-node 28 (bo)))
(defun position-indexes-to-chess (position)
	(concatenate 'string (string (code-char (+ 65 (first position)))) (write-to-string (+ 1 (second position))))
)

(defun values-to-chess (values board)
(mapcar
      (lambda (value)
          (position-indexes-to-chess (position-node value board)))
    values)
)

(defun remove-simmetric-assimmetric (value board &optional (strategy 'max))
  (let* (
         (simmetric (reverse (write-to-string value)))
         )

    (cond
     ((null value) board)
     ((< value 10) (remove-node (* value 10) board))
     ((equal (parse-integer simmetric)  value) (remove-node (asymmetric-value board strategy) board))
     (t (remove-node (parse-integer simmetric) board))
     )
    )
  )

(defun asymmetric-value (board &optional (strategy 'max))
  (let* (
         (min-max (asymmetric-values board))
         )

    (cond ((null min-max) nil)
          (t (apply strategy min-max))
          )
    )
)

(defun asymmetric-values (board)
  (apply #'append
         (mapcar
          (lambda (n)
            (cond
             ((or (not (typep n 'real)) (< n 10)) nil)
             ((not(equal (parse-integer (reverse (write-to-string n)))  n)) nil)

             (t (list n))
             )
            )

          (remove-nil-value board)
          )
				)
  )

	(defun remove-nil-value (board)
  (apply #'append
         (mapcar
          (lambda (lin)
            (apply #'append
                   (mapcar
                    (lambda (n)
                      (cond
                       ((or (not (typep n 'real))) nil)
                       (t (list n))
                       )
                      )
                    lin )
                   )
            )
          board)
         )
  )

(defun remove-node (value board)
    (let* (
           (line-column (position-node value board))
           )

      (cond ((or (null (first line-column)) (null (second line-column))) board)
            (t (replace-value (first line-column) (second line-column) board))
            )
      )
    )


(defun position-node (value board)
  (let* (
         (line (line-node value board))
         )

    (list (position line board :test #'equal) (position value line :test #'equal))
    )
  )

(defun value-node (line-index column-index board)
  (let* (
         (line (nth line-index board))
         (value (nth column-index line))
         )

    (cond
     ((or (null value) (< value 0)) nil)
     (t (list value))
     )
    )
  )

(defun line-node (value board)
  (apply #'append
         (mapcar
          (lambda (lin)
            (cond
             ((position value lin :test #'equal) lin)
             (T nil)
             )
            )
          board
       )
    )
  )

(defun replace-position (column-index line &optional (val nil))
  (cond
   ( (null line) '())
   ( (eq column-index 0) (cons val (cdr line)))
   ( (cons (car line) (replace-position (- column-index 1) (cdr line) val))))
  )


(defun replace-value (line-index column-index board &optional (val nil))
  (cond
   ((null board) '())
   ((or (equal line-index nil) (equal column-index nil)) nil)
   ( (equal line-index 0) (cons (replace-position column-index (nth line-index board) val) (cdr board)))
   ( (cons (car board) (replace-value (- line-index 1) column-index (cdr board) val))))
  )


(defun generate-moves (board player points-1 points-2)
(let* (
      (line-column (position-node player board))
      (line-index (first line-column))
      (column-index (second line-column))
      (board-no-player (replace-value line-index column-index board nil))
      )

      (cond ((null board-no-player) '())
      (t (append
        (move-avaliable (- line-index 2) (- column-index 1) board-no-player player points-1 points-2)
        (move-avaliable (- line-index 2) (+ column-index 1) board-no-player player points-1 points-2)
        (move-avaliable (+ line-index 2) (- column-index 1) board-no-player player points-1 points-2)
        (move-avaliable (+ line-index 2) (+ column-index 1) board-no-player player points-1 points-2)
        (move-avaliable (- line-index 1) (- column-index 2) board-no-player player points-1 points-2)
        (move-avaliable (- line-index 1) (+ column-index 2) board-no-player player points-1 points-2)
        (move-avaliable (+ line-index 1) (- column-index 2) board-no-player player points-1 points-2)
        (move-avaliable (+ line-index 1) (+ column-index 2) board-no-player player points-1 points-2)
        ))
      )
  )
)

(defun move-avaliable (line-index column-index board player points-1 points-2)
   (cond
   ((or (< line-index 0) (> line-index 9)) nil)
   ((or (< column-index 0) (> column-index 9)) nil)
   ((value-node line-index column-index board)

  ;;mudar para construtores

	(let* (
			(value (car (value-node line-index column-index board)))
			(points (sum-move player value points-1 points-2))
			)
   (list
     (list
		 	value
			(first points) ;;player -1
			(second points)	 ;;player -2
      (replace-value line-index column-index board player))
     )
		)
    )
   )
)

(defun max-first-move-value (player board)
  (let* (
      (line (if (equal -1 player) (first (bo)) (tenth (bo))))
      (max-value  (apply 'max line))
  )

    max-value
  )
)


(defun max-first-o-value (player board)
  (let* (
      (line (if (equal -1 player) (first (bo)) (tenth (bo))))
      (max-value  (apply 'max line))
      (indexes (position-node max-value board))
  )

    (replace-value (first indexes) (second indexes) board player)
  )
)

(defun get-play-time-milisecs(start-time)
  "gets the time of a made play"
    (- (get-internal-real-time) start-time)
)

(defun sum-move (player points points-1 points-2)
  (cond
    ((equal player -1) (list (+ points points-1) points-2))
    (t (list points-1 (+ points points-2)))
  )
)
