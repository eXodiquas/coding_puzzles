(defun read-file (path)
  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
	  while line
	  collect line)))

;;; DAY 5 - PART 1

(defun binary-seat-search (str size)
  (let ((sl (coerce str 'list)))
    (labels ((f (s hi lo)
	       (let ((mp (/ (+ hi lo) 2)))
		 (if (= (length s) 1)
		     (if (or (char= (car s) #\B) (char= (car s) #\R))
			 hi
			 lo)
		     (if (or (char= (car s) #\F) (char= (car s) #\L))
			 (f (cdr s) (floor mp) lo)
			 (f (cdr s) hi (ceiling mp)))))))
      (f sl size 0))))

(defun partition-seat (str)
  (list (subseq str 0 7) (subseq str 7 10)))

(defun generate-id (tpl)
  (+ (* (car tpl) 8) (cadr tpl)))

(defun run (path)
  (let* ((inp (read-file path))
	(tpls (mapcar
	       (lambda (x)
		 (list (binary-seat-search (car x) 127)
		       (binary-seat-search (cadr x) 7)))
	       (mapcar #'partition-seat inp))))
    (sort (mapcar #'generate-id tpls) #'>)))

(run "input.txt")

;;; DAY 5 - PART 2

(defun find-seat (ns)
  (labels ((f (n r)
	      (if (null n)
		  r
		  (if (null (cadr n))
		      r
		      (if (= (abs (- (car n) (cadr n))) 1)
			  (f (cdr n) r)
			  (f (cdr n) (cons (list (car n) (cadr n)) r)))))))
    (f ns '())))

(defun run-2 (path)
  (find-seat (run path)))

(run-2 "input.txt")
