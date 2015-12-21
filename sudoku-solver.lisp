;; Sudoku solver writte by Tom Callahan with the assistance of Alex Stults
;; September 2015

(defun read-formatted-sudoku-file (filename)
	(let ((lines '()) (in (open filename)))
  		(when in 
  			(loop for line = (read-line in nil)
  				while line do 
  					(setf lines 
  						(cons (mapcar #'digit-char-p (string-to-list line)) lines)))
	  		(close in)
  			(reverse lines))))

(defun read-raw-sudoku-file (filename)
	(let ((lines '()) (in (open filename)))
  		(when in 
  			(loop for line = (read-line in nil)
  				while line do
  					(let ((first-character (char line 0)))
  					  (if (eq first-character #\Space)
  						(push 0 lines)
  						(push (digit-char-p first-character) lines))))
	  		(close in)
  			(reverse lines))))

(defun pretty-print-board (list)
	 (format t "~%~a~%" (repeat-string "_" 37))
	 (loop for row in list
	 	do (progn 	(format t "~a|~%" (repeat-string "|   " 9)) 
	 				(format t "~{~a~}|~%" (mapcar (lambda (s) (format nil "| ~a " s)) row))
	 				(format t "~a|~%" (repeat-string "|___" 9)))))

(defun repeat-string (string n)
	(if (= n 0)
		""
		(concatenate 'string string (repeat-string string (- n 1)))))

(defun string-to-list (s)
   (coerce s 'list))

(defun retrieve-row (row-index board)
	"retrieve a row from the board"
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
		board
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
	(cond ((invalid-state row column) nil)
		  ((coord-is-immutable row column board master-board)
				(move-backward (previous-row row column) 
					   		   (previous-col column) 
					   		   board))
		  ((move-availablep row column board)
				(make-move row 
						   column
						   (next-valid-move row column board) 
						   board))
		  (t (move-backward (previous-row row column) 
							(previous-col column)
							(make-move row column 0 board)))))

(defun invalid-state (row column)
	(or (< row 0) (< column 0)))

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

(defun list-to-board (list)
	(if (null list)
		'()
		(cons (up-to-nth 10 list)
				(list-to-board (rest-nth 9 list)))))
 
(setf my-board (list-to-board (read-raw-sudoku-file "raw_sudoku.txt")))

(setf master-board
	(list-to-board (read-raw-sudoku-file "raw_sudoku.txt")))

(pretty-print-board (bt-solve-board my-board))