;;; INPUT TRANSFORMATION

(defun read-file (path)
  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
	  while line
	  collect line)))

(defun sanitize-input (path)
  (mapcar (lambda (x) (coerce (string-trim '(#\) x) 'list)) (read-file path)))

;;; DAY 11 - PART 1

(defun input->hash (path)
  (let ((hash (make-hash-table :test #'equal))
	(inp (sanitize-input path)))
    (loop for h below (length inp) do
      (loop for w below (length (car inp)) do
	(setf (gethash (list h w) hash) (nth w (nth h inp)))))
    hash))

(defun get-neighbours (pos world)
  (let ((x (car pos))
	(y (cadr pos)))
    (list
     (gethash (list (1- x) (1+ y)) world #\.)
     (gethash (list (1- x)      y) world #\.)
     (gethash (list (1+ x) (1+ y)) world #\.)
     (gethash (list (1- x)      y) world #\.)
     (gethash (list (1+ x)      y) world #\.)
     (gethash (list     x  (1- y)) world #\.)
     (gethash (list (1- x) (1- y)) world #\.)
     (gethash (list (1+ x) (1- y)) world #\.))))

(defun count-occupied (pos world)
  (count #\# (get-neighbours pos world)))

(defun count-free (pos world)
  (count #\L (get-neighbours pos world)))

(defun count-all-occupied (hash)
  (let ((counter 0))
    (loop for k being the hash-keys in hash using (hash-value v) do
      (if (char= v #\#)
	  (setf counter (1+ counter))))
    counter))

(defun hash-table-equal (ht1 ht2)
  (and (= (hash-table-count ht1) (hash-table-count ht2))
       (loop for k1 being the hash-keys in ht1 using (hash-value v)
	     always (equal v (gethash k1 ht2)))))

(defun next-hash (hash)
  (let ((next-gen (make-hash-table :test #'equal)))
    (loop for k being the hash-keys in hash using (hash-value v) do
      (cond ((char= v #\#) (if (>= (count-occupied k hash) 4)
			       (setf (gethash k next-gen) #\L)
			       (setf (gethash k next-gen) v)))
	    ((char= v #\L) (if (= (count-occupied k hash) 0)
			       (setf (gethash k next-gen) #\#)
			       (setf (gethash k next-gen) v)))
	    (t (setf (gethash k next-gen) v))))
    next-gen))

(defun simulate (hash)
  (let* ((fst hash)
	 (snd (next-hash hash)))
    (if (hash-table-equal fst snd)
	(count-all-occupied trd)
	(simulate snd))))
