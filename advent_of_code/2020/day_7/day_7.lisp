(defun read-file (path)
  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
	  while line
	  collect line)))

;;; DAY 7 - PART 1

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
  (let ((bag (make-instance 'bag :name (format nil "~a ~a" (car bag-string) (cadr bag-string)))))
    (loop for (am c1 c2) on (cddr bag-string) by #'cdddr
	  
	  do (add-bag bag (make-instance 'bag :name (format nil "~a ~a" c1 c2) :amount am)))
    bag))

(defun input->datastructure (path)
  (mapcar #'create-data-structure (sanitize-input path)))

(defclass bag ()
  ((name
    :initarg :name
    :accessor bag-name)
   (parent
    :initarg :parent
    :accessor bag-parent)
   (contains
    :initarg :contains
    :initform '()
    :accessor bag-contains)
   (amount
    :initarg :amount
    :initform 1
    :accessor bag-amount)))

(defmethod add-bag ((big bag) (small bag))
  (setf (bag-contains big) (cons small (bag-contains big))))
