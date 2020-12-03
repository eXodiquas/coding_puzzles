(defun read-file (path)
  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
	  while line
	  collect line)))

;;; DAY 3 - PART 1

(defun get-collisions (path)
  (let ((file (read-file path)))
    (loop for l in file
	  for c from 0 by 3
	  collect (char (string-trim '(#\Return) l) (rem c (1- (length l)))))))

(defun count-collisions (path)
  (count #\# (get-collisions path)))

(defun run (path)
  (count-collisions path))

(run "input.txt")

;;; DAY 3 - PART 2

(defun get-collisions-2 (path right)
  (let ((file (read-file path)))
    (loop for l in file
	  for c from 0 by right
	  collect (char (string-trim '(#\Return) l) (rem c (1- (length l)))))))

(defun get-collisions-2-skipping (path)
  (let ((file (read-file path)))
    (loop for l in file by #'cddr
	  for c from 0
	  collect (char (string-trim '(#\Return) l) (rem c (1- (length l)))))))


(defun count-collisions-2 (path)
  (* (count #\# (get-collisions-2 path 1))
     (count #\# (get-collisions-2 path 3))
     (count #\# (get-collisions-2 path 5))
     (count #\# (get-collisions-2 path 7))
     (count #\# (get-collisions-2-skipping path))))

(defun run-2 (path)
  (count-collisions-2 path))

(run-2 "input.txt")
