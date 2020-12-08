;;; INPUT TRANSFORMATION

(defun read-file (path)
  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
	  while line
	  collect line)))

(defun sanitize-input (path)
  (let* ((in (read-file path)))
    (mapcar #'transform-to-keyword-value (mapcar #'uiop:split-string (mapcar (lambda (x) (string-trim '(#\) x)) in)))))

(defun transform-to-keyword-value (tpl)
  `(,(read-from-string (concatenate 'string ":" (car tpl))) ,(read-from-string (cadr tpl))))

;;; DAY 8 - PART 1

(defun run (path)
  (let ((input (sanitize-input path)))
    (labels ((f (ptr acc prev)
	       (if (find ptr prev)
		   acc
		   (let ((curr-com (car (nth ptr input)))
			 (curr-val (cadr (nth ptr input))))
		     (cond ((equal curr-com :nop) (f (1+ ptr) acc (push ptr prev)))
			   ((equal curr-com :acc) (f (1+ ptr) (+ acc curr-val) (push ptr prev)))
			   ((equal curr-com :jmp) (f (+ ptr curr-val) acc (push ptr prev))))))))
      (f 0 0 '()))))

(run "input.txt")

;;; DAY 8 - PART 2

(defun terminatesp (input)
  (labels ((f (ptr acc prev)
	     (if (find ptr prev)
		 nil
		 (if (= ptr (length input))
		     acc
		     (let ((curr-com (car (nth ptr input)))
			   (curr-val (cadr (nth ptr input))))
		       (cond ((equal curr-com :nop) (f (1+ ptr) acc (push ptr prev)))
			     ((equal curr-com :acc) (f (1+ ptr) (+ acc curr-val) (push ptr prev)))
			     ((equal curr-com :jmp) (f (+ ptr curr-val) acc (push ptr prev)))))))))
    (f 0 0 '())))

(defun toggle-key (kv)
  (cond ((equal (car kv) :nop) `(:jmp ,(cadr kv)))
	((equal (car kv) :jmp) `(:nop ,(cadr kv)))
	(t kv)))

(defun replace-nth (n lst with)
  (labels ((f (n cur lst with res)
	     (if (null lst)
		 res
		 (if (= n cur)
		     (f n (1+ cur) (cdr lst) with (cons with res))
		     (f n (1+ cur) (cdr lst) with (cons (car lst) res))))))
    () (f n 0 lst with '())))

(defun run-2 (path)
  (let* ((input (sanitize-input path)))
    (car (remove-if #'null (loop for i below (length input)
				 collect (terminatesp (replace-nth i input (toggle-key (nth i input)))))))))
