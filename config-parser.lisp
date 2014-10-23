(defpackage :config-parser
  (:use :cl)
  (:export #:slither-config-parse)
  (:export #:slither-dump-board))

(provide :config-parser)
(in-package :config-parser)

#| 
 Reads the data from the file; constructs puzzle/data-structure and returns the puzzle.
|#

(defun make-adjustable-string (s)
               (make-array (length s)
                           :fill-pointer (length s)
                           :adjustable t
                           :initial-contents s
                           :element-type (array-element-type s)))

(defun make-grid-nodes (cols) 
  (setq s (make-adjustable-string ""))
  (loop repeat cols do (vector-push-extend #\+ s) (vector-push-extend #\  s)) (vector-push-extend #\+ s) 
  (return-from make-grid-nodes s))

(defvar *board* NIL)

(defun slither-config-parse (file)
  (let ((in (open file))
	(local-board NIL))
    (when in 
      (let ((rows-cols (read in)))
	    (setq rows (car rows-cols))
	    (setq cols (cadr rows-cols))
	(loop for line = (read-line in nil)
	       while line do 
		 (setf local-board (append local-board (list (make-grid-nodes cols))))
		 (setf local-board (append local-board (list (substitute #\  #\, (concatenate 'string (concatenate 'string " " line) " "))))))
	(setf local-board (append local-board (list  (make-grid-nodes cols)))))
	    (close in))
    (setq *board* local-board)
    *board*))

(defun slither-dump-board ()
  (format t "~%")
  (dolist (i *board*) (format t "~a~%" i)))

(defun make-move ()
 )

(slither-config-parse "input.txt")
(slither-dump-board)

     
