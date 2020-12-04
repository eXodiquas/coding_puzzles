(defun read-file (path)
  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
	  while line
	  collect line)))

(defparameter *contents* '("byr"
			   "iyr"
			   "eyr"
			   "hgt"
			   "hcl"
			   "ecl"
			   "pid"))

;;; DAY 4 - PART 1

(defun parse-input (path)
  (let ((data (read-file path)))
    (labels ((f (d current res)
	       (if (not d)
		   (cons current res)
		   (if (string-equal (car d) "\")
		       (progn
			 (f (cdr d) '() (cons current res)))
		       (f (cdr d) (cons (car d) current) res)))))
      (f data '() '()))))

(defun create-passport (strs)
  (loop for s in strs
	nconc (loop for ss in (uiop:split-string s)
		    collect (subseq ss 0 3))))

(defun cidp (pp)
  (string= pp "cid"))

(defun check-passportp (pp)
  (null (set-exclusive-or (remove-if 'cidp pp) *contents* :test 'equalp)))

(defun run (path)
  (let* ((formatted-list (parse-input path))
	 (passport-list (mapcar 'create-passport formatted-list)))
    (count t (mapcar 'check-passportp passport-list))))

(run "input.txt")

;;; DAY 4 - PART 2 (requires cl-ppcre for regex)

(defun flatten (l)
    (if (null l)
        nil
        (if (atom (first l))
            (cons (first l) (flatten (rest l)))
            (append (flatten (first l)) (flatten (rest l))))))

(defun sanitize-input (path)
  (loop for p in (parse-input path)
	collect (flatten (mapcar 'uiop:split-string p))))

(defun passport->hash (passport)
  (let ((passport-hash (make-hash-table :test 'equal))
	(kv (mapcar (lambda (x) (uiop:split-string x :separator '(#\:))) passport)))
    (loop for x in kv do
      (setf (gethash (car x) passport-hash) (string-trim '(#\) (cadr x))))
    passport-hash))

(defun is-validp (passporthash)
  (and (<= 1920 (parse-integer (gethash "byr" passporthash "-1")) 2002)
       (<= 2010 (parse-integer (gethash "iyr" passporthash "-1")) 2020)
       (<= 2020 (parse-integer (gethash "eyr" passporthash "-1")) 2030)
       (heightcheckp (gethash "hgt" passporthash nil))
       (colorcheckp (gethash "hcl" passporthash nil))
       (eyecolorcheckp (gethash "ecl" passporthash nil))
       (pidcheckp (gethash "pid" passporthash nil))))

(defun heightcheckp (height)
  (if (null height)
      nil
      (let ((end (subseq height (- (length height) 2))))
	(if (string= end "cm")
	    (<= 150 (parse-integer height :junk-allowed t) 193)
	    (if (string= end "in")
		(<= 59 (parse-integer height :junk-allowed t) 76))))))

(defun colorcheckp (color)
  (if (> (length color)7)
      nil
      (ppcre:scan "#[0-9a-f]{6}?" color)))

(defun eyecolorcheckp (color)
  (find color '("amb" "blu" "brn" "gry" "grn" "hzl" "oth") :test 'equal))

(defun pidcheckp (number)
  (if (> (length number) 9)
      nil
      (ppcre:scan "[0-9]{9}" number)))

(defun run-2 (input)
  (let* ((sanin (sanitize-input input))
	(pphashes (mapcar 'passport->hash sanin)))
    (count 0 (mapcar 'is-validp pphashes))))
