
(in-package :fundamental-period)
(ql:quickload :parse-float)
(ql:quickload :cl-csv)


(ql:quickload :cl-mathstats)
(defun linear-fit (&key (data (slot-value *table* 'list)))
  (multiple-value-bind (slope c r) 
      (cl-mathstats:linear-regression-brief (mapcar #'second  data)
					    (mapcar #'first data))
    (values (list (list "Slope" slope)
		  (list "Intercept" c)
		  (list "R^2 (Coeff. of Determination)" r))
	    slope c r)))

(ql:quickload :fit)

(defun fit (lambda initial &key (data (slot-value *table* 'list)))
  (fit::fit lambda data initial t t))

(defun multilinear-fit (&key (data (slot-value *table* 'list)))
  (fit (lambda (x a b c)
	 (+ a (* b (first x)) (* c (second x))))
       '(1 1 1)
       :data data))

(defun multilinear-origin-fit (&key (data (slot-value *table* 'list)))
  (fit (lambda (x b c)
	 (+ (* b (first x)) (* c (second x))))
       '(1 1)
       :data data))

(defun linear-origin-fit (&key (data (slot-value *table* 'list)))
  (fit (lambda (x a) (* a x)) '(1) :data data))

(defun power-fit (&key (data (slot-value *table* 'list)) (initial '(1 1)))
  (append (list (list "Equation =" "a*x^b"))
	  (fit::fit (lambda (x a b) (* a (expt x b))) data initial t t)))

(defun shifted-power-fit (&key (data (slot-value *table* 'list)) (initial '(1 1 1)))
  (append (list (list "Equation =" "a*x^b + c"))
	  (fit::fit (lambda (x a b c) (+ c(* a (expt x b)))) data initial t t)))

(defun almost-linear-fit (&key (data (slot-value *table* 'list)))
  (multiple-value-bind (table slope c r) (linear-fit :data data)
    (declare (ignore table r))
    (shifted-power-fit :data data :initial (list slope 1 c))))

(defun polynomial-fit2 (&key (data (slot-value *table* 'list)) (initial '(1 1)))
  (append (list (list "Equation =" "a*x^2 + b*x"))
	  (fit::fit (lambda (x a b) (+ (* b x) (* a (expt x 2)))) data initial t t)))


;; exponential fit  y = A*e^(Bx)
(defun exponential-fit (&optional (data (slot-value *table* 'list)))
  (multiple-value-bind (slope c r) 
      (cl-mathstats:linear-regression-brief (mapcar (lambda (d) (log (second d))) data) ;; ln(y)
					    (mapcar #'first data)) ;; x
    (list (list "A" (exp c))
	  (list "B" slope)
	  (list "R^2 (Coeff. of Determination)" r))))



(defun polynomial2-fit (&key (data (slot-value *table* 'list)) (initial '(1 1 1)))
  (append (list (list "Equation =" "a*x^2 + b*x + c "))
	  (fit::fit (lambda (x a b c) (+ c (* b x) (* a (expt x 2)))) data initial t t)))

(defun statistical-summary (data)
  (multiple-value-bind (length minimum maximum range median mode mean variance sd iqr skewness)
      (cl-mathstats:statistical-summary data)
    (list (list 'length length) (list 'minimum minimum)
	  (list 'range range)
	  (list 'maximum maximum)
	  (list 'median median)
	  (list 'mode mode)
	  (list 'mean mean)
	  (list 'variance variance)
	  (list 'sd sd)
	  (list 'iqr iqr)
	  (list 'skewness skewness))))




;;;;;
(defun fit-params (&optional (fit *lastfit*))
  (mapcar #'(lambda (n)
	      (cond ((numberp n) n)
		    (t (parse-float:parse-float n :type 'double-float :exponent-character #\d))))
	  (butlast (mapcar #'first (rest fit)) 3)))


(defun max-diff (fit &optional (data *bigdata*))
  (loop for (x tp) in data
	for fit-tp = (funcall fit x)
	maximizing (abs (- tp fit-tp))))

(defun max-%diff (fit &optional (data *bigdata*))
  (loop for (x tp) in data
	for fit-tp = (funcall fit x)
	maximizing (abs (* 100 (/ (- tp fit-tp) fit-tp)))))

(defun save-fit-func (func params)
  (lambda (x)
    (apply func x params)))

(defun max-fit-%diff (func initial &key (data *bigdata*) (save nil))
  (multiple-value-bind (params r^2 rmse covariance) (fit::fit func data initial t nil)
    (if save
	(setf (symbol-value save) (save-fit-func func params)))
    (table 6
      (row "max %diff" "R^2" "rmse" "Params" "Covariance")
      (row (loop for (x tp) in data
		 for fit-tp = (apply func x params)
		 maximizing (abs (* 100 (/ (- tp fit-tp) fit-tp))))
	   r^2
	   rmse
	   params
	   (loop for i from 0 below (length initial) collect (grid:aref covariance i i))))))

(defun map-data (func &optional (data *bigdata*))
  (loop for (x tp) in data do
    (funcall func x tp)))

(defun filter-data(&key n h bw bx by (data *bigdata*))
  (loop for d in data
	for (x tp) = d
	for (nn hh bwbw bxbx byby) = x
	when (and (or (not n) (= n nn))
		  (or (not h) (= h hh))
		  (or (not bw) (if (listp bw) (equal bw bwbw) (= bw bwbw)))
		  (or (not bx) (= bx bxbx))
		  (or (not by) (= by byby)))
	  collect d))

(defun fit-check-csv (fit-func x-func file &optional (data *bigdata*))
  (cl-csv:write-csv
   (sort (loop 
	   for (x tp) in data 
	   collect (list (funcall x-func x) tp (funcall fit-func x)))
	 #'< :key #'first)
   :stream (merge-pathnames file *img-path*)))

(defun scatter-csv (fit-func file &optional (data *bigdata*))
  (cl-csv:write-csv
   (loop for (x tp) in data 
	 for fitted-tp = (funcall fit-func x)
	 collect  (list tp fitted-tp))
   
   :stream (merge-pathnames file *img-path*)))

(defun scatter-plot (fit-func file &optional (data *bigdata*))
  (let ((tp-and-fit (loop for (x tp) in data 
			  for fitted-tp = (funcall fit-func x)
			  collect  (list tp fitted-tp))))
    (with-plot file
      (setup :xlabel "Exact T" :ylabel "Fitted T" 
	     :terminal '(:png :size :|1500,1100| :font "Times New Roman, 30pt"))
      (ezplot tp-and-fit :title "Fit" :with '(:point))
      (eazy-gnuplot:plot "x" :with '(:lines) :title "Exact"))))

(defun comparision-scatter-plot (fit-func other-funcs titles file &optional (data *bigdata*))
  (let ((tp-and-fit (loop for (x tp) in data 
			  for fitted-tp = (funcall fit-func x)
			  collect  (list tp fitted-tp)))
	(sorteddata (sort data #'< :key #'second)))
    (with-plot file
      (setup :xlabel "Exact T" :ylabel "Fitted T" 
	     :terminal '(:png :size :|1500,1100| :font "Times New Roman, 30pt"))
      (ezplot tp-and-fit :title "Fit" :with '(:point))
      (eazy-gnuplot:plot "x" :with '(:lines) :title "Exact")
      (loop for o in other-funcs
	    for title in titles do
	(ezplot (loop for (x tp) in sorteddata
		      for fitted-tp = (funcall o x)
		      collect (list tp fitted-tp))
		:title title
		:with :point)))))
		

(defun fit-plot (fit-func x-axis file &optional (data *bigdata*))
  (let ((x-tp-fit (sort (loop 
			  for (x tp) in data 
			  collect (list (funcall x-axis x) tp (funcall fit-func x)))
			#'< :key #'first)))
    (with-plot file
      (setup :xlabel "X-axis" :ylabel "Tp"
	     :terminal '(:png :size :|1500,1100| :font "Times New Roman, 30pt"))
      (ezplot x-tp-fit :title "Fit" :using '(1 3) :with '(:lines))
      (ezplot x-tp-fit :title "Exact" :using '(1 2) :with '(:point)))))

(defun /Nh (x)
  (* (first x) (second x)))

(defun sd1 (sn^2 sn n)
  "Stanard deviation"
  (sqrt (- (/ sn^2 n)
	   (expt (/ sn n) 2))))

(defun cv1 (sn^2 sn n)
  "Coefficient of variance"
  (/ (sd1 sn^2 sn n)
     (/ sn n)))

(defun sensitivity (param filtered-data)
  (let ((parami (1- (ecase param
		      (:n 1) (:h 2) (:bw 3) (:bx 4) (:by 5)))))
    (loop for (x tp) in filtered-data
	  for p = (nth parami x)
	  summing tp into stp
	  summing (expt tp 2) into  stp^2
	  summing p into sp
	  summing (expt p 2) into sp^2
	  summing 1 into count 
	  finally (return (/ (cv1 stp^2 stp count)
			     (cv1 sp^2  sp  count))))))

(defun remove-nth (n list)
  (nconc (subseq list 0 n) (nthcdr (1+ n) list)))
(defparameter *bigdata* nil)

(defun max-effect (param &optional (data *bigdata*))
  (let ((parami (1- (ecase param
		      (:n 1) (:h 2) (:bw 3) (:bx 4) (:by 5) (:nh 6) (:bxby 7))))
	(table (make-hash-table :test 'equal)))
    (loop for (x tp) in data
	  for p =  (if (> parami 4) nil (nth parami x))
	  for others = (cond ((= parami 5) (nthcdr 2 x))
			     ((= parami 6) (butlast (butlast x)))
			     (t (remove-nth parami x)))
	  for (min max) = (or (gethash others table) '(100 0)) do
	    (setf (gethash others table) (list (min min tp) (max max tp))))
    (loop for o being the hash-key of table
	  for (min max) = (gethash o table)
	  with maxdiff = 0
	  with maxo = nil do
	    (when (<= maxdiff (- max min))
	      (setf maxdiff (- max min)
		    maxo o))
	  finally (return (values maxdiff maxo)))))
    

(defun histogram (list &key (min (reduce #'min list)) (max (reduce #'max list)) (steps 1))
  (let* ((size (ceiling (- max min) steps))
	 (index (mapcar (lambda (n)
			  (min (max  0 (floor (- n min) steps)) size))
			list))
	 (histogram (make-array size :element-type 'fixnum :initial-element 0)))
    (loop for i in index do
      (incf (aref histogram i)))
    histogram))


(defun histogram-list (list &key (min (reduce #'min list)) (max (reduce #'max list)) (steps 1))
  (let ((hist (histogram list :min  min :max  max :steps steps)))
    (loop for v across hist
	  for x from min by steps
	  collect (list x v))))

(defun read-csv (filename)
  (let (bigdata)
    (cl-csv:read-csv (merge-pathnames filename *img-path*)
		     :data-map-fn #'(lambda (data &key csv-reader)
				      (declare (ignore csv-reader))
				      (let (( *read-default-float-format* 'double-float))
					(read-from-string data)))
		     :row-fn #'(lambda (row)
				 (push (list (butlast row) (first (last row)))
				       bigdata)))
    bigdata))
