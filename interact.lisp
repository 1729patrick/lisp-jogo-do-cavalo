
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
              ((equal mode 'hc) (format t "hc") (format t (read-first-player)))
              ((equal mode 'cc) (format t "cc"))
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
            (format t "   ~%---------------------CHOOSE FIRST PLAYER----------------------------")
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
