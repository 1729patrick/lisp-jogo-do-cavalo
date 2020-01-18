
(defvar *base-pathname*
    (or *load-truename* *compile-file-truename*))

(defun asset-path
    (file)
    (merge-pathnames file *base-pathname*))

(progn
    (load
        (asset-path "algorithmn.lisp"))
    (load
        (asset-path "game.lisp")))



(defun start
    ()
    (let
        (
            (mode (read-mode)))
            (cond
              ((equal mode 'hc) (game (read-time-limit) (read-first-player)))
              ((equal mode 'cc) (game (read-time-limit) nil))
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
                (                    (eq answer 1) 'h)
                (                    (eq answer 2) 'c)

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


;;J5
(defun read-play ()
    (progn
        (progn
            (format t "~%1---------------------ENTER YOUR MOVE---------------------")
              (terpri)
)
        (read)
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
