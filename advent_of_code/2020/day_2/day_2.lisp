(defun read-file (path)
  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
	  while line
	  collect line)))

;;; DAY 2 - PART 1

(defun parse-line (line)
  (let* ((splitted (uiop:split-string line :separator " "))
	 (minc (parse-integer (car (uiop:split-string (car splitted) :separator "-"))))
	 (maxc (parse-integer (cadr (uiop:split-string (car splitted) :separator "-"))))
	 (chr (char (cadr splitted) 0))
	 (pwd (caddr splitted)))
    (list minc maxc chr pwd)))

(defun satisfyp (pwd chr minc maxc)
  (let ((cnt (count chr pwd)))
    (if (and (>= cnt minc) (<= cnt maxc))
	t
	nil)))

(defun check-line (line)
  (destructuring-bind (minc maxc chr pwd) (parse-line line)
    (satisfyp pwd chr minc maxc)))

(defun run (path)
  (length (remove-if-not #'check-line (read-file path))))

(run "input.txt")

;;; DAY 2 - PART 2

(defun satisfy2p (pwd chr fst snd)
  (let ((c1 (char pwd (- fst 1)))
	(c2 (char pwd (- snd 1))))
    (or (and (char= chr c1) (not (char= chr c2)))
	(and (char= chr c2) (not (char= chr c1))))))

(defun check-line2 (line)
  (destructuring-bind (fst snd chr pwd) (parse-line line)
    (satisfy2p pwd chr fst snd)))

(defun run2 (path)
  (length (remove-if-not #'check-line2 (read-file path))))

(run2 "input.txt")
