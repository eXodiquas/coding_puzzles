;;; INPUT TRANSFORMATION

(defun read-file (path)
  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
	  while line
	  collect line)))

(defun sanitize-input (path)
  (let ((inp (read-file path)))
    (list
     (parse-integer (car inp) :junk-allowed t)
     (remove-if
      #'null
      (mapcar
       (lambda (x)
	 (parse-integer x :junk-allowed t))
       (uiop:split-string (cadr inp) :separator '(#\,)))))))

;;; DAY 13 - PART 1

(defun calc-delay (n1 n2)
  (* n2 (ceiling (/ n1 n2))))

(defun closest (path)
  (let* ((inp (sanitize-input path))
	 (n (car inp))
	 (l (cadr inp)))
    (labels ((f (nu li re bu)
	       (if (null li)
		   (* (- re n) bu)
		   (let ((nr (calc-delay nu (car li))))
		     (if (>= nr re)
			 (f nu (cdr li) re bu)
			 (f nu (cdr li) nr (car li)))))))
      (f n l (calc-delay n (car l)) (car l)))))

(defun run (path)
  (closest path))

(run "input.txt")

;;; DAY 13 - PART 2

