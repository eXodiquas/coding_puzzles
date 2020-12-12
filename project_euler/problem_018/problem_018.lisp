;;; INPUT

(defun read-file (path)
  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
	  while line
	  collect line)))

;;; INPUT TRANSFORMATION

(defun sanitize-input (path)
  (mapcar
   (lambda (x)
     (string-trim '(#\) x))
   (read-file path)))

(defun listify-input (path)
  (mapcar #'uiop:split-string (sanitize-input path)))

(defun numberfy-input (path)
  (mapcar (lambda (x)
	    (mapcar #'parse-integer x))
	  (listify-input path)))

;;; SOLUTION

(defun get-index (upper-line index)
  (let ((left (max (1- index) 0))
	(right (min index (- (length upper-line) 2))))
    (list (nth left upper-line) (nth right upper-line))))

(defun calculate-max-path (path)
  (let ((input (reverse (numberfy-input path))))
    (labels ((f (current-line upper-line future-lines current-pos)
	       (if (null future-lines)
		   current-line
		   (f
		    (mapcar (lambda (x)
			      (let ((vals (get-index upper-line current-pos)))
				(print vals)
				(cond ((null (car vals)) (cadr vals))
				      ((null (cadr vals)) (car vals)))
				(max (+ x (car vals)) (+ x (cadr vals)))))
			    upper-line)
		    (car future-lines)
		    (cdr future-lines)
		    (1+ current-pos)))))
      (f (car input) (cadr input) (caar input) 0))))
