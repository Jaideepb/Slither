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
(defvar *rows* NIL)
(defvar *cols* NIL)
(defvar *game-over-flag* NIL)


(defun slither-config-parse (file)
  (let ((in (open file))
	(local-board NIL))
    (when in 
      (let ((rows-cols (read in)))
	    (setq *rows* (car rows-cols))
	    (setq *cols* (cadr rows-cols))
	(loop for line = (read-line in nil)
	       while line do 
		 (setf local-board (append local-board (list (make-grid-nodes *cols*))))
		 (setf local-board (append local-board (list (substitute #\  #\, (concatenate 'string (concatenate 'string " " line) " "))))))
	(setf local-board (append local-board (list  (make-grid-nodes *cols*)))))
	    (close in))
    (setq *board* local-board)
    *board*))

(defun slither-dump-board ()
  (format t "~%")
  (dolist (i *board*) (format t "~a~%" i)))

(defun read-move () 
  (loop 
     (when *game-over-flag*  ; Game-over flag 
       (print "Game Over") (return)) 
     (parse-check-move (write-to-string (read)))))

(defun parse-check-move (move)
  ;; move is of the format RCE (Row-Col-Edge)
  (cond ((= (length move) 5)
	 ;; Check if the move is valid. 
	 
	 (Examine-Execute-Move move)
	 (slither-dump-board))

	 #|(if (> (incf num-moves) 5) 
	     (setq *flag* t))) |#
	(t (print "Wrong move values")) ))

(defun Examine-Execute-Move (move)
  (Calculate-move move)
  (set-edge (nth 1 gridnum))
  (setq *game-over-flag* t))

(defun Calculate-move (mv)
  (let (( b (coerce mv 'list)))
	(setq row (digit-char-p (nth 1 b)))
	(setq col (digit-char-p (nth 2 b)))
	(setq edge (nth 3 b))
    (cond ((and (<= row *rows*) (<= col *cols*) (if (member edge '(#\U #\L #\R #\D)) t nil))
	   (setq gridnum (Get-Cell-Numbers row col edge))
	   (print gridnum)
	   (return-from Calculate-move gridnum))
	  (t (print "Invalid move")))))

(defun Get-Cell-Numbers (row col edge)
  ; should return a list '(cell-num edge-num node-num)
  (setq cell-num (+ (* (- (* 2 row) 1)  (+ (* 2 *cols*) 1)) (* 2  col)))
  (cond ((char-equal edge #\U)
	 (setq edge-num (- cell-num (+ (* 2 *cols*) 1)))
 	 (setq node-num (list (- edge-num 1) (+ edge-num 1))))

	((char-equal edge #\L)
	 (setq edge-num (- cell-num 1))
	 (setq node-num (list (- edge-num (+ (* 2 *cols*) 1)) (+ edge-num (+ (* 2 *cols*) 1)))))

	((char-equal edge #\R)
	 (setq edge-num (+ cell-num 1))
	 (setq node-num (list (- edge-num (+ (* 2 *cols*) 1)) (+ edge-num (+ (* 2 *cols*) 1)))))

	((char-equal edge #\D)
	 (setq edge-num (+ cell-num (+ (* 2 *cols*) 1)))
	 (setq node-num (list (- edge-num 1) (+ edge-num 1)))))

  (list cell-num edge-num node-num))

(defun set-edge (edge)
  (setq temp-cols (+ (* *cols* 2) 1))
  (let ((r (floor (/ edge temp-cols)))
	(c (mod edge temp-cols)))
    (if (= c 0) (setq c temp-cols))
    (decf c)
    (if (= (mod r 2) 1)
	(setf (char (nth r *board*) c) #\|)
	(setf (char (nth r *board*) c) #\-))))
	
  

(slither-config-parse "input.txt")
(slither-dump-board)

(setq *game-over-flag* NIL)     
(read-move)


(defun test ()

  (setq abc (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
  (setf (nth 0 (nth 2 abc)) 10) 
  (print abc))

(test)
