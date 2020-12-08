;;; DAY 7 - INPUT TRANSFORMATION

(defun read-file (path)
  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
	  while line
	  collect line)))

(defun sanitize-input (path)
  (let* ((in (read-file path))
	(cleaned (mapcar
		    (lambda (x) (string-trim '(#\) x))
		    (car (uiop:split-string in :separator '(#\))))))
    (mapcar #'minimize-input cleaned)))

(defun minimize-input (string)
  (remove-if (lambda (x) (or
			  (equal x "contain")
			  (equal x "bags")
			  (equal x "bag,")
			  (equal x "bags,")
			  (equal x "bags.")))
	     (uiop:split-string string)))

(defun create-data-structure (bag-string)
  (cons (list (car bag-string) (cadr bag-string)) (loop for (am c1 c2) on (cddr bag-string) by #'cdddr	  
							collect (list am c1 c2))))

(defun make-color (colortupel)
  (if (= (length colortupel) 2)
      (concatenate 'string (car colortupel) " " (cadr colortupel))
      (list (parse-integer (car colortupel) :junk-allowed t) (concatenate 'string (cadr colortupel) " " (caddr colortupel)))))

(defun input->datastructure (path)
  (let ((bag-list (mapcar #'create-data-structure (sanitize-input path)))
	(bag-hash-table (make-hash-table :test #'equal)))
    (loop for bg in bag-list do
      (let ((rep (mapcar #'make-color bg)))
	(setf (gethash (car rep) bag-hash-table) (cdr rep))))
    bag-hash-table))

(defparameter *database* (input->datastructure "input.txt"))

;;; DAY 7 - PART 1

(defun containsp (cont col)
  (let ((cur (gethash cont *database*)))
    (some (lambda (c)
	    (or (string= (cadr c) col)
		(containsp (cadr c) col)))
	  cur)))

(defun run ()
  (loop for k being the hash-key using (hash-value v) of *database* count (containsp k "shiny gold")))

;;; DAY 7 - PART 2

