;;board-order
(defun bo()
  '(
    (00 01 02 03 04 05 06 07 08 09)
    (10 11 12 13 14 15 16 17 18 19)
    (20 21 22 23 24 25 26 27 28 29)
    (30 31 32 33 34 35 36 37 38 39)
    (40 41 42 43 44 45 46 47 48 49)
    (50 51 52 53 54 55 56 57 58 59)
    (60 61 62 63 64 65 66 67 68 69)
    (70 71 72 73 74 75 76 77 78 79)
    (80 81 82 83 84 85 86 87 88 89)
    (90 91 92 93 94 95 96 97 98 99)
    )
)

(defun bo-2()
  '(
    (00 01 02 03 04 05 06 07 08 09)
    (10 11 -1 13 14 15 16 17 18 19)
    (20 21 22 23 24 25 26 27 28 29)
    (30 31 32 33 34 35 36 37 38 39)
    (40 41 42 43 44 45 46 -2 48 49)
    (50 51 52 53 54 55 56 57 58 59)
    (60 61 62 63 64 65 66 67 68 69)
    (70 71 72 73 74 75 76 77 78 79)
    (80 81 82 83 84 85 86 87 88 89)
    (90 91 92 93 94 95 96 97 98 99)
    )
)

(defun error-board ()

'(
  (0 NIL NIL NIL NIL NIL NIL NIL NIL NIL)
  (NIL NIL NIL NIL 14 NIL 16 NIL 18 NIL)
  (NIL -1 NIL NIL NIL 25 NIL 27 28 29)
  (30 NIL 32 NIL 34 NIL 36 NIL 38 39)
  (NIL NIL NIL 43 NIL 45 NIL 47 NIL 49)
  (NIL NIL NIL NIL 54 NIL 56 NIL 58 -2)
  (60 61 NIL 63 NIL 65 NIL 67 68 69)
  (70 71 72 NIL 74 NIL 76 77 78 79)
  (NIL 81 NIL 83 84 85 86 87 88 89)
  (90 91 92 93 94 95 96 97 98 NIL)
)
)


(defun lista-numeros (&optional (n 100))
  (if (< n 1) '() (cons (1- n) (lista-numeros(1- n))))
)
;;teste: (lista-numeros)
;; resultado: (99 98 97 96 95 94 93 92 91 90 89 88 87 86 85 ... 5 4 3 2 1 0)

;;;função que baralha uma lista de numeros utilizando rotação entre o primeiro elemento da lista e outro aleatorio
(defun baralhar (input-list &optional accumulator)
  "Shuffle a list using tail call recursion."
  (if (eq input-list nil)
      accumulator
      (progn
	(rotatef (car input-list) (nth (random (length input-list)) input-list))
	(baralhar (cdr input-list) 
				 (append accumulator (list (car input-list))))
				 )))
;;teste: (baralhar (lista-numeros))
;;resultado: outputs muito variados
	
;;;função que gera um tabuleiro aleatorio	
(defun tabuleiro-aleatorio (&optional (lista (baralhar (lista-numeros))) (n 10))
	(cond
		((null lista) nil)
		(t (cons (subseq lista 0 n) (tabuleiro-aleatorio (subseq lista n) n)))
	)
)
;;teste: (tabuleiro-aleatorio)	 
;;resultado: outputs muito variados->   ((3 9 82 94 14 54 86 39 71 97) 
										;;(61 90 42 84 93 62 51 96 45 38) 
										;;(19 76 0 17 37 1 60 40 31 30) 
										;;(67 99 27 55 32 95 57 28 24 6) 
										;;(72 33 79 81 49 73 36 69 16 43) 
										;;(89 66 56 65 48 4 75 47 91 87) 
										;;(68 64 22 53 70 41 50 15 5 52) 
										;;(29 77 35 26 13 34 74 2 44 21) 
										;;(59 7 18 78 88 8 85 83 25 11) 
										;;(58 20 10 63 92 80 23 12 46 98))
;;<node>= current-points, points-of-player1, points-of-player2, <state> f player
;;;<state>= current-board
(defun make-node(points p-p1 p-p2 board f player depth)
  "Devolve um novo nó"
  (list points p-p1 p-p2 board f player depth)
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

(defun node-f(node)
  (fifth node)
)

(defun node-player(node)
  (sixth node)
)

(defun node-depth(node)
  (seventh node)
)

;;<play-node>= <state> f move(cell-points) player
;;;<state>= current-board
(defun make-play-node(board f move player)
  (list board f move player)
)

(defun play-node-board(play-node)
  (first play-node)
)

(defun play-node-f(play-node)
  (second play-node)
)

(defun play-node-move(play-node)
  (third play-node)
)

(defun play-node-player(play-node)
  (fourth play-node)
)

;;Node value heuristic
(defun evaluate-node(points-p1 points-p2 player)
 "returns the value of the node between -1 and 1, knowing that the total points in the board are 4950"

   (cond 
      ((equal player -2) 
          (cond
            ((> (- points-p2 points-p1) 2475) 1)
            ((< (- points-p2 points-p1) -2475) -1)
            ((equal (- points-p2 points-p1) 0) 0)
            (t   (round-to (float (/ (- points-p2 points-p1) 4950)) 5))
          )          
      )

      (t 
            (cond
                ((> (- points-p1 points-p2) 2475) 1)
                ((< (- points-p1 points-p2) -2475) -1)
                ((equal (- points-p1 points-p2) 0) 0)
                (t   (round-to (float (/ (- points-p1 points-p2) 4950)) 5))
            )
          ) 
    )
)

(defun round-to (number precision &optional (what #'round))
    (let ((div (expt 10 precision)))
         (float (/ (funcall what (* number div)) div))))

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

(defun list-points (node-list)
  (cond ((null node-list) nil) 
     (t (cons (node-points (car node-list)) (list-points (cdr node-list))))
  )
)

(defun remove-point-list-nil(point-list)
  (apply #'append
                   (mapcar 
                    (lambda (n) 
                      (cond
                       ((or (not (typep n 'real))) nil)
                       (t (list n))
                       )
                      )
                    point-list )
                   )
            
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


"""(defun generate-moves (board player)
(let* (
      (line-column (position-node player board))
      (line-index (first line-column))
      (column-index (second line-column))
      (board-no-player (replace-value line-index column-index board nil))
      )

      (cond ((null board-no-player) '())
      (t (append
        (move-avaliable (- line-index 2) (- column-index 1) board-no-player player)
        (move-avaliable (- line-index 2) (+ column-index 1) board-no-player player)
        (move-avaliable (+ line-index 2) (- column-index 1) board-no-player player)
        (move-avaliable (+ line-index 2) (+ column-index 1) board-no-player player)
        (move-avaliable (- line-index 1) (- column-index 2) board-no-player player)
        (move-avaliable (- line-index 1) (+ column-index 2) board-no-player player)
        (move-avaliable (+ line-index 1) (- column-index 2) board-no-player player)
        (move-avaliable (+ line-index 1) (+ column-index 2) board-no-player player)
        ))
      )
  )
)"""



(defun sum-move (player points points-1 points-2)
  (cond
    ((equal player -1) (list (+ points points-1) points-2))
    (t (list points-1 (+ points points-2)))
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

(defun test-node ()
"points p-p1 p-p2 board f player depth"
  (make-node 2 10 15 (bo-2) 0.002 -1 0)
)

(defun test-node-2 ()
"points p-p1 p-p2 board f player depth"
  (make-node 2 10 15 (bo-2) 0.8 -1 0)
)

(defun test-node-3 ()
"points p-p1 p-p2 board f player depth"
  (make-node 2 10 15 (bo-2) 1 -1 0)
)

(defun opposite (player)
(cond
      ((equal player -1) -2)
      (t -1)
    )
)

(defun sort-f-nodes (node-list)
  (cond
    ((null node-list) nil)
    (t
      (append
        (sort-f-nodes (list> (car node-list) (cdr node-list)))
        (cons (car node-list) nil) 
        (sort-f-nodes (list<= (car node-list) (cdr node-list)))))))

(defun list> (a b)
  (cond
    ((or (null a) (null b)) nil)
    ((> (node-f a) (node-f (car b))) (list> a (cdr b)))
    (t (cons (car b) (list> a (cdr b))))))

(defun list<= (a b)
  (cond
    ((or (null a) (null b)) nil)
    ((<= (node-f a) (node-f(car b))) (list<= a (cdr b)))
    (t (cons (car b) (list<= a (cdr b))))))