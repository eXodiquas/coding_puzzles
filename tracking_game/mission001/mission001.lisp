(defun get-file (path)
  "opens a file at path and reads line by line"
  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
	  while line
	  collect line)))

(defun get-first-n (seq n)
  "returns the first n elements of a sequence."
  (let ((l (coerce seq 'list)))
      (labels ((f (seq n res)
		 (if (zerop n)
		     res
		     (f (rest seq) (1- n) (cons (first seq) res)))))
	(reverse (f l n '())))))

(defun duplicatesp (seq)
  "checks if sequence contains any duplicates."
  (not (= (length seq) (length (remove-duplicates (sort seq #'char-lessp))))))

(defun step-through-seq (seque n)
  "steps through every n items long subsequence and checks if there are duplicates in there, if not the subsequences are added to the result list."
  (let* ((seq (coerce seque 'list)))
    (labels ((f (s r)
	       (let ((first-n (get-first-n s n)))
		 (if (or (not s) (some #'null first-n))
		     r
		     (if (not (duplicatesp first-n))
			 (f (rest s) (cons first-n r))
			 (f (rest s) r))))))
      (f seq '()))))

(print (concatenate 'string (car (step-through-seq (car (get-file "input.txt")) 16))))
