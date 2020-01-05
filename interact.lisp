
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
              ((equal mode 'hc) (format t "hc") (format t (read-first-player)) (format t (read-time-limit)))
              ((equal mode 'cc) (format t "cc") (format t (read-time-limit)))
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

(defun display-computer-move (move board)
  (terpri)
  (format t "Computer move: ~a" move)
  (terpri)
  (terpri)
  (display-board board)
)

(defun read-move ()
  (terpri)
  (format t "Enter your move: ")
  (read)
)
