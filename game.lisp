(defun position-chess-to-indexes (position)
"returns a positition converted into a line and column indexes"
	(list (- (char-code (character (string-upcase (subseq position 0 1)))) 65) (parse-integer (remove (character (subseq position 0 1)) position)))
)
