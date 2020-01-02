;;board-order
(defun bo()
  '(
    (0 1 2 3 4 5 6 7 8 9)
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

(defun position-chess-to-indexes (position)
"returns a positition converted into a line and column indexes"
	(list (- (char-code (character (string-upcase (subseq position 0 1)))) 65) (parse-integer (remove (character (subseq position 0 1)) position)))
)

(defun max-min-asymmetric-value (board &optional (strategy 'max))
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
