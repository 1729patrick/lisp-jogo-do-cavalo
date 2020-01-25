
(defvar *base-pathname*
    (or *load-truename* *compile-file-truename*))

(defun asset-path
    (file)
    (merge-pathnames file *base-pathname*))

(progn
    (load
        (asset-path "game.lisp"))
    (load
        (asset-path "algorithmn.lisp")))



(defun start
    ()
    (let
        (
            (mode (read-mode))
            (depth (read-depth))
            )
            (cond
              ((equal mode 'hc) (game-hc (read-time-limit) (read-first-player) depth))
              ((equal mode 'cc) (game-cc (read-time-limit) depth))
              (t (format t "Invalid mode"))

            )
)
)


(defun read-mode
    ()
    (progn
        (progn
            (format t "   ~%---------------------CHOOSE A MODE----------------------------")
            (terpri)
            (format t "   ~%                 1 - Human vs Computer                        ")
            (format t "   ~%                 2 - Computer vs Computer                  ")
            (terpri)
            (format t "   ~% --------------------------------------------------------~%~%> ")
)

        (let
            (
                (answer
                    (read)))
            (cond
                (                    (eq answer 1) 'hc)
                (                    (eq answer 2) 'cc)

))
)
)


(defun read-first-player ()
    (progn
        (progn
            (format t "   ~%---------------------CHOOSE THE FIRST PLAYER----------------------------")
            (terpri)
            (format t "   ~%                 1 - Human                                 ")
            (format t "   ~%                 2 - Computer                              ")
            (terpri)
            (format t "   ~% --------------------------------------------------------~%~%> ")
)

        (let
            (
                (answer
                    (read)))
            (cond
                (                    (eq answer 1) -1)
                (                    (eq answer 2) -2)

))
)
)


(defun read-time-limit ()
    (progn
        (progn
            (format t "   ~%------ENTER TIME FOR COMPUTER PLAY (1000 >= TIME [ms] >= 5000)------")
            (terpri)
)
        (read)
)
)

(defun read-depth ()
    (progn
        (progn
            (format t "   ~%------ENTER THE MAXIMUM DEPTH------")
            (terpri)
)
        (read)
)
)

;;J5
(defun read-play (moves-available)
    (terpri)
    (format t "Enter your move ~a: " moves-available)

    (let (
        (play (write-to-string (read)))
       )

    (cond
      ((null (position play moves-available :test #'equal))
        (format t "Invalid move :(")
        (terpri)
        (read-play moves-available)
      )
      (t play)
     )
    )
)


(defun display-board (board)
    (cond
        ((null board) '())
        (t
            (write-line (write-to-string (car board)))
            (display-board (cdr board))
        )
    )
)

;; (defun add-zero-left (line)
;; (mapcar
;;       (lambda (value)
;;           (cond
;;             ((or (null value) (> value 0)) value)
;;             ((< value 10) (concatenate 'string "0" (write-to-string value)))
;;             (t (write-to-string value))
;;           )
;;       )
;;     line)
;; )

(defun display-computer-move (computer move board)
  (terpri)
  (terpri)
  (format t "Computer ~a move: ~a" computer move)
  (terpri)
  (terpri)
  (display-board board)
  (terpri)
)

(defun display-points-players (points-player-1 points-player-2)
  (format t "Total points player -1: ~a" points-player-1)
  (terpri)
  (format t "Total points player -2: ~a" points-player-2)
)


(defun display-start-board (board)
(terpri)
(terpri)
(format t "----------------------START BOARD-----------------")
(terpri)
(terpri)
(display-board board)
(terpri)
(format t "----------------------START BOARD-----------------")
(terpri)
(terpri)
)

(defun statistics-file-path()
  "gets the path for of the log file"
  (asset-path "log.dat")
  )

  (defun write-log (board points-1 points-2 play-time num-analized-nodes)
  "Writes the play log, along with -- o número de nós analisados, o número de cortes efetuados (de cada tipo), o tempo gasto
em cada jogada e o tabuleiro atual."

    (with-open-file (file (statistics-file-path) :direction :output :if-exists :append :if-does-not-exist :create)
           (progn 
             (terpri)
                (format file "~%~t------------:PLAY MADE------------")
             (terpri)
                (format file "~%~t----:  Playing Time: ~a Miliseconds" play-time)
             (terpri)
                (format file "~%~t----:  Points Player-1: ~a" points-1)
             (terpri)
                (format file "~%~t----:  Points Player-2: ~a" points-2)
  
             (terpri)
             (terpri)
                (format file "~%~t----:  Current Board:")
             (terpri)
                (print-board board file)
             )
    )
  )

  (defun print-board(board &optional (file-stream t))
  "lista a board"
  (not (null (mapcar #'(lambda(line) 
        (format file-stream "~%~t~t ~a" line)) board)))
  )