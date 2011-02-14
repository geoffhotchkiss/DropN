(defun welcome (name)
  (format t "Welcome to drop7, ~a!" name))

(defvar *rowlist* nil)

(defvar *size* 7)

(defun make-row ()
  (let ((row nil))
    (dotimes (n *size*)
      (push (random (1+ *size*)) row))
    row))

(defun add-row (row)
  (setf *rowlist* (append *rowlist* (list row))))
  
(defun clear-rowlist ()
  (setf *rowlist* nil))

(defun print-rowlist ()
  (format t "Current game:~%")
  (loop for row in *rowlist*
       do
       (format t "~{~d ~}~%" row)))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun ask-for-place (num)
  (or
   (parse-integer (prompt-read (format nil "Where would you like to place ~d" num)) :junk-allowed t)
   -1))

(defun add-at-place (num place)
  (format t "Adding ~d at ~d~%" num place)
  (setf (nth place (find-list place)) num))

; this really doesn't want to work right...
; should find a way to make this work
; without the loop i have currently..
(defun find-list (place)
  (let ((thelist nil))
    (loop for i from 0 to (1- (length *rowlist*))
       do
	 (let ((row (nth i *rowlist*)))
	   (if (/= (nth place row) 0)
	       (break (progn
		 (format t "At ~d~%" i)
		 (setf thelist (nth (1- i) *rowlist*)))))))
    (format t "Out of there")
    (if (not (equal thelist nil))
	(nth (1- (length *rowlist*)) *rowlist*)
	thelist)))
			 
  
(defun play-game ()
  (clear-rowlist)
  (print-rowlist)
  (dotimes (n *size*)
    (add-row '(0 0 0 0 0 0 0)))
  ;(print-rowlist)
  (let* ((num (random (1+ *size*)))
	 (done? nil))
    (loop while (not done?)
	 do
	 (print-rowlist)
	 (let ((place (ask-for-place num)))
	   (if (= place -1)
	       (progn
		 (format t "Quiting")
		 (setf done? t))
	       (add-at-place num place))
	       
	   (setf num (random (1+ *size*)))))))

		    
