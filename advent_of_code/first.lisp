(defun read-file (path)
  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
	  while line
	  collect line)))

;;; DAY 1 - PART 1

(defun get-content-as-numbers (path)
  (mapcar #'parse-integer (read-file path)))

(defun create-2020-tuple (path)
  (let ((c (get-content-as-numbers path)))
    (loop for i in c
	  nconc (loop for j in c
			when (= (+ i j) 2020)
			  collect (list i j)))))

(defun create-result (path)
  (let ((r (car (create-2020-tuple path))))
    (apply '* r)))

(create-result "input.txt")

;;; DAY 1 - PART 2

(defun create-2020-triple (path)
  (let ((c (get-content-as-numbers path)))
    (loop for i in c
	    nconc (loop for j in c
			  nconc (loop for k in c
					when (= (+ i j k) 2020)
					  collect (list i j k))))))

(defun create-result-triple (path)
  (let ((r (car (create-2020-triple path))))
    (apply '* r)))

(create-result-triple "input.txt")
