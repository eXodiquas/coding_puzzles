;;; INPUT TRANSFORMATION

(defun read-file (path)
  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
	  while line
	  collect line)))

(defun sanitize-input (path)
  (mapcar #'parse-integer (read-file path)))

;;; DAY 10 - PART 1

(defun add-build-in-jolt (path)
  (let ((jolts (sort (sanitize-input path) '>)))
    (cons 0 (sort (cons (+ 3 (car jolts)) jolts) '<))))

(defun make-differences (path)
  (let ((jolts (add-build-in-jolt path)))
    (mapcar (lambda (x) (apply #'- x)) (mapcar #'list jolts (cdr jolts)))))

(defun count-differences (path)
  (let ((diffs (make-differences path)))
    (list
     (count -1 diffs)
     (count -3 diffs))))

(defun run (path)
  (apply #'* (count-differences path)))

(run "input.txt") ; 2100

;;; DAY 10 - PART 2

(defun add-build-in-jolt-2 (path)
  (let ((jolts (sort (sanitize-input path) '>)))
    (sort (cons (+ 3 (car jolts)) jolts) '<)))

;;; muchas gracias: https://www.reddit.com/r/adventofcode/comments/ka8z8x/2020_day_10_solutions/gf9c493?utm_source=share&utm_medium=web2x&context=3
(defun memoization (inp)
  (let ((mem (make-hash-table :test #'equal)))
    (labels ((f (p nums)
	       (let* ((c (first nums))
		      (key (list p c)))
		 (cond ((null nums) 0)
		       ((> (- c p) 3) 0)
		       ((null (rest nums)) 1)
		       ((gethash key mem) (gethash key mem))
		       (t (setf (gethash key mem)
				(+ (f p (rest nums))
				   (f c (rest nums)))))))))
      (f 0 inp))))

(defun run-2 (path)
  (memoization (add-build-in-jolt-2 path)))

(run-2 "input.txt")
