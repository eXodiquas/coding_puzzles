;;; INPUT TRANSFORMATION

(defun read-file (path)
  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
	  while line
	  collect line)))

(defun sanitize-input (path)
  (mapcar #'parse-integer (read-file path)))

(defun list-first-n (list n)
  (loop for ele in list
	for i below n
	collect ele))

;;; DAY 9 - PART 1

(defun all-sums (numbers)
  (remove-duplicates
   (sort
    (loop for i in numbers append
			   (loop for j in numbers collect
						  (+ i j)))
			   #'<)))

(defun check-sum (path input-size)
  (labels ((f (current-list n)
	     (if (< (length current-list) n)
		 :eol
		 (if (find
		      (nth n current-list)
		      (all-sums (list-first-n current-list n)))
		     (f (cdr current-list) n)
		     (nth n current-list)))))
    (f (sanitize-input path) input-size)))

(defun run (path n)
  (check-sum path n))

(run "input.txt" 25) ;1639024365

;;; DAY 9 - PART 2

(defun find-con-sets (path number)
  (let ((input (sanitize-input path))
	(results '()))
    (loop for i from 1 below (length input) do
      (loop for j from i below (length input) do
	(if (= (apply #'+ (subseq (list-first-n input j) i)) number)
	    (setf results (cons (subseq (list-first-n input j) i) results)))))
    results))

(defun longest-con-set (path number)
  (let* ((consets (find-con-sets path number))
	 (conset (sort (car (sort consets '> :key #'length)) '>)))
    (+ (car conset) (car (last conset)))))

(defun run-2 (path n)
  (longest-con-set path n))

(run-2 "input.txt" (run "input.txt" 25)) ;219202240 runs in about 5 seconds
