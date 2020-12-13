;;; INPUT TRANSFORMATION

(defun read-file (path)
  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
	  while line
	  collect line)))

(defun sanitize-input (path)
  (mapcar (lambda (x)
	    (list
	     (coerce (subseq x 0 1) 'character)
	     (parse-integer (subseq x 1) :junk-allowed t)))
	  (read-file path)))

;;; DAY 12 - PART 1

(defun move-ship (path)
  (labels ((f (commands direction x y)
	     (if (null commands)
		 (list (abs x) (abs y))
		 (let* ((dir (caar commands))
			(amt (cadar commands))
			(dir-rad (* direction (/ pi 180)))
			(x-rad (* amt (cos dir-rad)))
			(y-rad (* amt (sin dir-rad))))
		   (cond ((char= dir #\N) (f (cdr commands) direction x (+ y amt)))
			 ((char= dir #\S) (f (cdr commands) direction x (- y amt)))
			 ((char= dir #\E) (f (cdr commands) direction (+ x amt) y))
			 ((char= dir #\W) (f (cdr commands) direction (- x amt) y))
			 ((char= dir #\L) (f (cdr commands) (rem (+ direction amt) 360) x y))
			 ((char= dir #\R) (f (cdr commands) (rem (- direction amt) 360) x y))
			 ((char= dir #\F) (f (cdr commands) direction (+ x x-rad) (+ y y-rad))))))))
    (f (sanitize-input path) 0 0 0)))

(defun run (path)
  (truncate (apply #'+ (move-ship path))))

(print (run "input.txt"))
