(defun read-file (path)
  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
	  while line
	  collect line)))

;;; DAY 6 - PART 1

(defun sanitize-input (path)
  (let ((i (read-file path)))
    (labels ((f (i cur res)
	       (if (null i)
		   (cons cur res)
		   (if (string-equal (car i) "\")
		       (f (cdr i) '() (cons cur res))
		       (f (cdr i) (cons (string-trim '(#\) (car i)) cur) res)))))
      (f i '() '()))))

(defun count-occ (slst)
  (let ((hash (make-hash-table)))
    (loop for ele in slst do
      (loop for c across ele do
	(setf (gethash c hash) 1)))
    (hash-table-count hash)))

(defun run (path)
  (apply #'+ (mapcar #'count-occ (sanitize-input path))))

(run "input.txt")

;;; DAY 6 - PART 2

(defun hash-table-val-increment (ht key)
  (if (null (gethash key ht))
      (setf (gethash key ht) 1)
      (setf (gethash key ht) (1+ (gethash key ht)))))

(defun count-occ-2 (gr)
  (let ((hash (make-hash-table)))
    (loop for ele in gr do
      (loop for c across ele do
	(hash-table-val-increment hash c)))
    (let ((kv-pairs (loop for k being each hash-key of hash
			    using (hash-value v) collect (list k v))))
      (length  (remove-if-not (lambda (x) (= (cadr x) (length gr))) kv-pairs)))))

(defun run-2 (path)
  (apply #'+ (mapcar #'count-occ-2 (sanitize-input path))))

(run-2 "input.txt")
