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

(defun set-hashmap (id ln r)
  (print id)
  (print ln)
  (print r)
  (let ((idxx1 0))
    (loop for i from (+ (* (+ (* 2 *cols*) 1) id) 1)
       while (< i (+ (* (+ (* 2 *cols*) 1) (+ id 1)) 1))
       do (cond ((= r -1) 
		 (setf (gethash i *grid-hash*) 0))
	       	(t (cond ((and (char-equal (char ln idxx1) #\  ) (= (mod idxx1 2) 0))
			  (setf (gethash i *grid-hash*) 0))
			 ((and (char-equal (char ln idxx1) #\  ) (= (mod idxx1 2) 1))
			  (setf (gethash i *grid-hash*) 9))
			 (t (setf (gethash i *grid-hash*) (digit-char-p (char ln idxx1)))
			    (if (/= (digit-char-p (char ln idxx1)) 0)
			    (pushnew i *incomplete-cells*))))))
	 (incf idxx1))))
	
(defun print-idx (ix)
  (print ix))

(defun slither-config-parse (file)
  (let ((in (open file))
	(local-board NIL))
    (when in 
      (let ((rows-cols (read in))
	    (idx 0))
	    (setq *rows* (car rows-cols))
	    (setq *cols* (cadr rows-cols))
	    (setq *min-gridnum* 1)
	    (setq *max-gridnum* (* (+ (* 2 *rows*) 1) (+ (* 2 *cols*) 1)))
	    (loop for line = (read-line in nil)
	       while line do 
     		 (incf idx)
		 (setf local-board (append local-board (list (make-grid-nodes *cols*))))
		 (set-hashmap (- idx 1) NIL -1)
		 (setf tt (substitute #\  #\, (concatenate 'string (concatenate 'string " " line) " ")))
		 (set-hashmap idx tt 1)
		 (incf idx)
		 (setf local-board (append local-board (list tt))))
	    (set-hashmap idx NIL -1)
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
	 (slither-dump-board)
	 (print *positive-edges*)
	 (print *incomplete-cells*))

	(t (print "Wrong move values"))))

;Positive edge list
;Completed-node list
(defun AdjustList (gnum)
  (let ((cell1 (nth 0 (nth 0 gnum)))
	(cell2 (nth 1 (nth 0 gnum)))
	(edg (nth 1 gnum))
	(node1 (nth 0 (nth 2 gnum)))
	(node2 (nth 1 (nth 2 gnum))))

    (if (= (gethash node1 *grid-hash*) 1)
	(pushnew node1 *incomplete-nodes*)
	(setq *incomplete-nodes* (delete node1 *incomplete-nodes*)))
    
    (if (= (gethash node2 *grid-hash*) 1)
	(pushnew node2 *incomplete-nodes*)
	(setq *incomplete-nodes* (delete node2 *incomplete-nodes*)))
    
    (if (= (gethash edg *grid-hash*) 1)
	;(add-into postive-edge list)
	(pushnew edg *positive-edges*)
	;(remove from positive-edge list))
	(setq *positive-edges (remove edg *positive-edges*)))
    (if (= (gethash cell1 *grid-hash*) 0)
	;if not present in incomplete-cell list add into it
	(progn (setq *incomplete-cells* (delete cell1 *incomplete-cells*))
	       (print "here"))
	;remove from incomplete-cell list
	(if (< (gethash cell1 *grid-hash*) 4) (pushnew cell1 *incomplete-cells*)))
    (if (not (null cell2))
	(progn 
	  (if (= (gethash cell2 *grid-hash*) 0)
	;if not present in incomplete-cell list add into it
	      (progn (setq *incomplete-cells* (delete cell2 *incomplete-cells*))
		     (print "here"))
	;remove from incomplete-cell list
         
	      (if (< (gethash cell2 *grid-hash*) 4) 
		  (pushnew cell2 *incomplete-cells*)))))))
	 
(defun AdjustHashMap (gnum)
  (let ((cell1 (nth 0 (nth 0 gnum)))
	(cell2 (nth 1 (nth 0 gnum)))
	(edg (nth 1 gnum))
	(first-n (nth 0 (nth 2 gnum)))
	(second-n (nth 1 (nth 2 gnum))))
    (cond ((= (gethash edg *grid-hash*) 0)
	   (incf (gethash first-n *grid-hash*))
	   (incf (gethash second-n *grid-hash*))
	   (incf (gethash edg *grid-hash*))
	   (decf (gethash cell1 *grid-hash*))
	   (if (not (null cell2)) 
	       (decf (gethash cell2 *grid-hash*))))
  
	  (t 
	   (decf (gethash first-n *grid-hash*))
	   (decf (gethash second-n *grid-hash*))
	   (decf (gethash edg *grid-hash*))
	   (incf (gethash cell1 *grid-hash*))
	   (if (not (null cell2)) 
	       (incf (gethash cell2 *grid-hash*)))))))

(defun Circle-check ()
  (let ((l-pos-edges (list-copy *positive-edges*))
	(first-edge (nth 0 l-pos-edges)))
    (setq l-pos-edges (delete first-edge l-pos-edges))
    (setq cur-edg first-edge)
    (let ((cur-nei (GetNeighbourEdges cur-edg)))
;;; Use a lambda here to 
      (loop for edg in cur-nei
	   do (if (member edg l-pos-edges)
		  (progn (setq l-pos-edges (delete edg l-pos-edges))
			 (setq cur-nei (GetNeighbourEdges edg)))))
      

(defun trail(i)
  (cond ((eq i 0)
	 (return-from trail (list 4 5 6)))
	((eq i 1)
	 (return-from trail (list 7 8 9)))
	((eq i 2)
	 (return-from trail (list 10 11 12)))
	((eq i 3)
	 (return-from trail (list 13 14 15)))
	(t 
	 (return-from trail NIL))))

(defun test2()
  (let ((i 0)
	(xyz (list 1 2 3)))
    (loop for edg in xyz
       do
	 (incf i)
	 (print edg)
	 (setq xyz (trail i))
	 (print xyz))))
	      
(test2)

(defun Node-check (gnum)
  (let ((edg (nth 1 gnum))
	(first-n (nth 0 (nth 2 gnum)))
	(second-n (nth 1 (nth 2 gnum))))
    (cond ((= (gethash edg *grid-hash*) 0)
	   (if (or (= (gethash first-n *grid-hash*) 2) (= (gethash second-n *grid-hash*) 2))
	       (progn 
		 (print  "Node-check: Invalid Move")
		 nil)
	       t))
	  (t t))))

(defun pass-tests (gnum)
  (Node-check gnum))

(defun check-game-over ()
  (if (and (= (length *incomplete-cells*) 0) (= (length *incomplete-nodes*) 0))
      t nil))
  
(defun Examine-Execute-Move (move)
  (let ((gnum (Calculate-move move)))
    (if (and (not (null gnum)) (pass-tests gnum))
	(progn 
	  (AdjustHashMap gnum)
	  (print "11")
	  (AdjustList gnum)
	  (print "22")
	  (if (= (gethash (nth 1 gnum) *grid-hash*) 0)
	      (set-edge (nth 1 gnum) 0)
	      (set-edge (nth 1 gnum) 1))
	  (if (check-game-over) (setq *game-over-flag* t)) ))))

(defun Calculate-move (mv)
  (let (( b (coerce mv 'list))
	(gridnum nil))
	(setq row (digit-char-p (nth 1 b)))
	(setq col (digit-char-p (nth 2 b)))
	(setq edge (nth 3 b))
    (cond ((and (<= row *rows*) (<= col *cols*) (if (member edge '(#\U #\L #\R #\D)) t nil))
	   (setq gridnum (Get-Cell-Numbers row col edge))
	   ;(setq gridnum (list (list 10 20) 22 (list 12 33)))
	   (print gridnum)
	   (return-from Calculate-move gridnum))
	  (t (print "Invalid move")
	     (return-from Calculate-move NIL)))))

(defun Get-Cell-Numbers (row col edge)
  ; should return a list '(cell-num edge-num node-num)
  (print (list *min-gridnum* *max-gridnum*))
  (let ((cell-num (+ (* (- (* 2 row) 1)  (+ (* 2 *cols*) 1)) (* 2  col)))
	(cell2 NIL))
    (cond ((char-equal edge #\U)
	   (setq edge-num (- cell-num (+ (* 2 *cols*) 1)))
	   (setq node-num (list (- edge-num 1) (+ edge-num 1)))
	   (let ((temp-cell2 (- edge-num (+ (* 2 *cols*) 1))))
	     (if (>= temp-cell2 *min-gridnum*)
		 (setq cell2 temp-cell2))))

	  ((char-equal edge #\L)
	   (setq edge-num (- cell-num 1))
	   (setq node-num (list (- edge-num (+ (* 2 *cols*) 1)) (+ edge-num (+ (* 2 *cols*) 1))))
	   (if (/= col 1)
	       (setq cell2 (- cell-num 2))))

	  ((char-equal edge #\R)
	   (setq edge-num (+ cell-num 1))
	   (setq node-num (list (- edge-num (+ (* 2 *cols*) 1)) (+ edge-num (+ (* 2 *cols*) 1))))
	   (if (/= col *cols*)
	       (setq cell2 (+ cell-num 2))))

	  ((char-equal edge #\D)
	   (setq edge-num (+ cell-num (+ (* 2 *cols*) 1)))
	   (setq node-num (list (- edge-num 1) (+ edge-num 1)))
	   (let ((temp-cell2 (+ edge-num (+ (* 2 *cols*) 1))))
	     (if (<= temp-cell2 *max-gridnum*)
		 (setq cell2 temp-cell2)))))

    (list (list cell-num cell2) edge-num node-num)))

(defun set-edge (edge flag)
  ; flag=0 ->unset , flag=1 ->set
  (print "HERE")
  (setq temp-cols (+ (* *cols* 2) 1))
  (let ((r (floor (/ (- edge 1) temp-cols)))
	(c (mod edge temp-cols)))
    (if (= c 0) (setq c temp-cols))
    (decf c)
    (if (= (mod r 2) 1)
	(progn 
	  (if (= flag 1)
	      (setf (char (nth r *board*) c) #\|)
	      (setf (char (nth r *board*) c) #\  )))
	(progn 
	  (if (= flag 1) 
	      (setf (char (nth r *board*) c) #\-)
	      (setf (char (nth r *board*) c) #\  ))))))
	
(defparameter *grid-hash* (make-hash-table))
(defvar *board* NIL)
(defvar *rows* NIL)
(defvar *cols* NIL)
(defvar *game-over-flag* NIL)
(defvar *positive-edges* NIL)
(defvar *incomplete-cells* NIL)
(defvar *incomplete-nodes* NIL)
(defvar *min-gridnum* NIL)
(defvar *max-gridnum* NIL)

(slither-config-parse "input2.txt")
(slither-dump-board)
(read-move)

(setq *rows* NIL)
(setq *cols* NIL)
(setq *incomplete-cells* NIL)
(setq *incomplete-nodes* NIL)
(setq *positive-edges* NIL)
(setq *game-over-flag* NIL)     
(setq *min-gridnum* NIL)
(setq *max-gridnum* NIL)




(defun test ()
  (defparameter *h* (make-hash-table))
  (loop for i from 1 
     when (< i 11)
     do (setf (gethash i *h*) 0)
       (print "xyz")
       (print "1")
       (print "2")
       (print "3")))


(test)
