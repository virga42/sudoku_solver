;; Sudoku solver writte by Tom Callahan with the assistance of Alex Stults
;; September 2015

(defun read-sudoku-file (filename)
	(let ((lines '()) (in (open filename)))
  		(when in 
  			(loop for line = (read-line in nil)
  				while line do 
  					(setf lines 
  						(cons (mapcar #'digit-char-p (string-to-list line)) lines)))
	  		(close in)
  			(reverse lines))))

(defun string-to-list (s)
   (coerce s 'list))

; retrieve a row from the board
(defun retrieve-row (row-index board)
	(nth row-index board))

; retrieve a column from the board 
(defun retrieve-column (column-index board)
	(if (null board)
		'()
		(cons (nth column-index (car board))
			(retrieve-column column-index (cdr board)))))

; retrieve a box from the board
(defun retrieve-box (box-index lst)
	(retrieve-box-helper (col-index-helper box-index) 
						 (slice-list (row-index-helper box-index) 3 lst)))

(defun col-index-helper (box-index)
	(* (mod box-index 3) 3))

(defun row-index-helper (box-index)
	(* (floor box-index 3) 3))

(defun row-in-coord (coord)
	(car coord))

(defun column-in-coord (coord)
	(cadr coord))

(defun retrieve-box-helper (column-index lst)
	(if (null lst)
		'() 
		(append (slice-list column-index 3 (car lst)) 
			  (retrieve-box-helper column-index (cdr lst)))))

(defun slice-list (index size lst)
	(if (= size 0)
		'()
		(if (= index 0)
			(cons (car lst) (slice-list 0 (- size 1) (cdr lst)))
			(slice-list 0 size (rest-nth index lst)))))

(defun rest-nth (index lst)
	(if (= index 0)
		lst
		(rest-nth (- index 1) (cdr lst))))

(defun up-to-nth (index lst)
	(if (= index 1)
		'()
		(cons (car lst) (up-to-nth (- index 1) (cdr lst)))))

; determine if a number is valid for a given position
(defun valid-movep (number row column board)
	(and (not (member number (retrieve-row row board)))
		 (not (member number (retrieve-column column board)))
		 (not (member number (retrieve-box (return-box-index row column) board)))))

(defun return-box-index (row column)
	(+ (floor column 3) (* (floor row 3) 3)))

(defun bt-stopp (board available-moves)
	(or (board-is-fullp board) (null available-moves)))

(defun value-of-coordinate (row column board)
	(nth column (nth row board)))

(defun next-valid-move (row column board)
	;change function to return one value
	(let ((current-value (value-of-coordinate row column board)))
		(loop for i from 1 to 9 
			when (and (valid-movep i row column board) (> i current-value))
				return i)))

(defun move-availablep (row column board)
	(next-valid-move row column board))

(defun bt-solve-board (board)
	(if (board-is-fullp board)
		(print board)
		(let* ((coord (get-next-empty-move board)) 
			   (row (row-in-coord coord))
			   (column (column-in-coord coord)))
			(if (move-availablep row column board)
				(bt-solve-board (make-move row 
										   column 
										   (next-valid-move row column board) 
										   board))
				(bt-solve-board (move-backward row column board))))))

(defun make-move (row column value board)
	(if (= row 0)
		(append (list (replace-value value column (car board))) (cdr board))
		(cons (car board) (make-move (- row 1) column value (cdr board)))))

(defun replace-value (value column lst)
	(if (= column 0)
		(append (list value) (cdr lst))
		(cons (car lst) (replace-value value (- column 1) (cdr lst)))))
 
(defun move-backward (row column board)
	(let ((prev-row (previous-row row column))
		  (prev-col (previous-col column)))
	(if (not (coord-is-immutable prev-row prev-col board master-board))
		(if (not (move-availablep prev-row prev-col board))
				(move-backward prev-row 
							   prev-col 
							   (make-move prev-row prev-col 0 board))
				(make-move prev-row 
						   prev-col
						   (next-valid-move prev-row prev-col board) 
						   board))
		(move-backward prev-row 
					   prev-col 
					   board))))

(defun previous-col (column)
	(if (= column 0)
		8
		(- column 1)))

(defun previous-row (row column)
	(if (= column 0)
		(- row 1)
		row))

(defun coord-is-immutable (row column board master-board)
	(= (value-of-coordinate row column board) (value-of-coordinate row column master-board)))

(defun board-is-fullp (board)
	(not (member 0 (board-to-list board))))

(defun get-next-empty-move (board)
	(block outer-loop
		(loop for row in board
		  for i = 0 then (+ i 1)
		  do (loop for value in row
				   for j = 0 then (+ j 1)
		  when (= value 0) 
		  do (return-from outer-loop (list i j))))))

(defun board-to-list (board)
	(if (null board)
		'()
		(append (car board) (board-to-list (cdr board)))))
 
; (print (read-sudoku-file "C:/Users/tcallahan/Dropbox/myDocuments/lisp/sudoku_board.txt"))

; (retrieve-row 0 (read-sudoku-file "C:/Users/tcallahan/Dropbox/myDocuments/lisp/sudoku_board.txt"))
; (retrieve-column 0 (read-sudoku-file "C:/Users/tcallahan/Dropbox/myDocuments/lisp/sudoku_board.txt"))
; (retrieve-box 2 (read-sudoku-file "C:/Users/tcallahan/Dropbox/myDocuments/lisp/sudoku_board.txt"))

; (let (box)
; 	slice-list 0 3 (slice-list 0 3 (read-sudoku-file "C:/Users/tcallahan/Dropbox/myDocuments/lisp/sudoku_board.txt"))

(setf master-board
	(read-sudoku-file "C:/Users/tcallahan/Dropbox/myDocuments/lisp/sudoku_board.txt"))

(setf test-board
	(read-sudoku-file "C:/Users/tcallahan/Dropbox/myDocuments/lisp/sudoku_board.txt"))
