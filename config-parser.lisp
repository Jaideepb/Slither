(defpackage :config-parser
  (:use :cl)
  (:export #:slither-config-parse)
  (:export #:slither-dump-board))

(provide :config-parser)
(in-package :config-parser)

#| 
 Reads the data from the file; constructs puzzle/data-structure and returns the puzzle.
|#

(defvar *board* NIL)

(defun slither-config-parse (file)
  (let ((in (open file))
	(local-board NIL))
    (when in 
      (let ((rows-cols (read in))
	    (rows (car rows-cols))
	    (cols (cadr rows-cols)))
	(loop for line = (read-line in nil)
	       while line do 
		 (setf local-board (append local-board (list "+ + + + + +")))
		 (setf local-board (append local-board (list line))))
	(setf local-board (append local-board (list "+ + + + + +"))))
	    (close in))
    (setq *board* local-board)))

(defun slither-dump-board ()
  (format t "~%")
      (dolist (i *board*) (format t "~a~%" i)))

(slither-dump-board)
#|
(defun parseFile (file)
  (let ((in (open file)))
    (when in
      (let ((dimens (read in))
	    (row (car dimens)))
	    ;(col (cadr dimens)))
	(print dimens))
	;(loop for line = (read-line in nil)
	 ;  while line do
	     ;(format t "~a~%" line)))
      (close in))))

(parseFile "input.txt")
#|
