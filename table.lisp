(push (truename (make-pathname :name  "../moi/" :type nil :defaults *load-pathname*)) asdf:*central-registry*)
(ql:quickload :moi)
(in-package :moi)
(ql:quickload :parse-float)
(ql:quickload :cl-csv)
(defparameter *table* nil)
(defparameter *table-float-digits* 3)
(defclass table ()
  ((list :initform nil :type list)
   (posi :initform 0 :type integer)
   (posj :initform 0 :type integer)))

(defun table-handle-floats (list)
  (cond ((listp list)
	 (mapcar #'table-handle-floats list))
	((typep list 'float)
	 (if *table-float-digits*
	     (format nil "~,vf" *table-float-digits* list)
	     list))
	((typep list 'number)
	 (if *table-float-digits*
	     (format nil "~,vf" *table-float-digits* (coerce list 'float))
	     list))
	(t list)))

(defmacro table (&body body)
  "first paramter can be t, nil, number or anything else"
  `(let ((*table* (make-instance 'table))
	 (*table-float-digits* ,(if (numberp (first body)) (first body) nil)))
     ,@(append body
	       (unless (eql (first body) nil)
		 (list `(table-handle-floats (slot-value *table* 'list)))))))

(defun table-data ()
  (slot-value *table* 'list))

(defun row (&rest values)
  (with-slots (list posi posj) *table*
    (setf list (append list (list values))
	  posi (1+ posi)
	  posj 0)))

(defun plot-printer (data &optional (type :xy) (format-string "~&~,10f ~,10f"))
  (if (typep type 'list)
      (lambda ()
	(loop for d in data
	      with xcol = (first type)
	      with ycol = (second type) do
		(format t format-string (nth xcol d) (nth ycol d))))
      (ecase type
	(:xy (lambda ()
	       (loop for (x y) in data do
		 (format t format-string x y))))
	(:xxyy (lambda ()
		 (loop for x in (first data)
		       for y in (second data) do
			 (format t format-string x y))))
	(:y (lambda ()
	      (loop for y in data
		    for i from 0 do
		      (format t format-string i y)))))))

(ql:quickload :eazy-gnuplot)
(defparameter *ezy-file* nil)
(defmacro with-plot (filename &body body)
  `(let ((*ezy-file* (merge-pathnames ,filename *img-path*)))
     (eazy-gnuplot:with-plots (*standard-output* :debug t)
       ,@body)
     (format nil "./img/~a" ,filename)))

(defun setup (&rest args &key (xlabel "x") (ylabel "y")
				(terminal "png")
				(key '(:bottom :right :font "Times New Roman,20"))
	      &allow-other-keys)
  
  (apply #'eazy-gnuplot:gp-setup :output *ezy-file*
				 :xlabel xlabel :ylabel ylabel
				 :terminal terminal
				 :key key 
				 (uiop:remove-plist-keys '(:xlabel :ylabel :terminal :key) args)))

(defun ezplot (data &rest args &key (title "Plot") (using '(1 2)) (with '(:lines))
	       &allow-other-keys)
  (apply #'eazy-gnuplot:plot (plot-printer data (mapcar #'1- using))
	 :using using
	 :with with
	 :title title
	 (uiop:remove-plist-keys '(:using :with :title) args)))

(defun plot-example () 
  (eazy-gnuplot:with-plots (*standard-output* :debug nil)
      (eazy-gnuplot:gp-setup :xlabel "x-label"      
			     :ylabel "y-label"
			     :output file 
			     :terminal "png"
			     :key '(:bottom :right :font "Times New Roman, 20")
			     :pointsize "0.4px")
      (eazy-gnuplot:plot (plot-printer data :xy)
			 :using '(1 2)
			 :title "title"
			 :with '(:lines))))

(defun plot (data file &key (type :xy) (gui nil) (multiple nil) title)
  (let ((terminal (if gui :qt :png)))
    (eazy-gnuplot:with-plots (*standard-output* :debug t)
      (eazy-gnuplot:gp-setup :xlabel "x-label"      ; strings : "\"x-label\""
			     :ylabel "y-label"
			     :output file ; pathnames : "\"sample.png\""
			     :terminal terminal         ; keyword/symbols: "terminal png"
			     
			     ;; list contents are recursively quoted, then joined by a space
			     :key '(:bottom :right :font "Times New Roman, 20")
			     
			     :pointsize "0.4px"
			     
			     ;;:yrange :|[0:1]|
			     ;; currently, specifying these kinds of options requires to abuse
			     ;; keywords and symbols. Another example: comma separated list, e.g.,
			     ;; :terminal '(:png :size |10cm,6cm|)
			     ;;
			     ;; 2/4/2016 Major options are now covered. 
			     )

      ;; any unsupported commands are available by printing it to the stream
      ;;(format t "~%unset key")
      
      ;; We are extending its expressivity. For example, as of 39d60d, there is gp-unset and gp-set.
      ;; An equivalent of above is (gp-unset :keys) .
      ;; The list is growing!

      ;; Functions can be protted with func-plot
      ;;(plot "sin(x)" :title "super sin curve!")
      ;; Plot a lisp data directly
      (eazy-gnuplot:plot (plot-printer data type)
			 :using '(1 2)
			 :title (if multiple (first title) title)
			 :with '(:lines))
      (when multiple
	(loop for i from 3 to (length (first data)) do 
	  (eazy-gnuplot:plot (plot-printer data (list 0 (1- i)))
			     :using '(1 2)
			     :title (nth (- i 2) title)
			     :with '(:lines))))
      (if gui
	  (format t "~&pause mouse button2;~%")))))

(defparameter *img-path* (asdf:system-relative-pathname :moi "../workbook/img/"))
(defun plot-table (&key file (type :xy) (gui nil) (title "line"))
  (plot (slot-value *table* 'list) (merge-pathnames file *img-path*) :type type :gui gui :multiple (listp title) :title title)
  (format nil "img/~a" file))

(defun plot2 (data filename &optional (titles "line"))
  (plot data (merge-pathnames filename *img-path*) :type :xy :gui nil :multiple (listp titles) :title titles)
  (format nil "img/~a" filename))

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
	  (butlast (mapcar #'first (rest fit)) 2)))


(defun max-diff (fit &optional (data *bigdata*))
  (loop for (x tp) in data
	for fit-tp = (funcall fit x)
	maximizing (abs (- tp fit-tp))))

(defun max-%diff (fit &optional (data *bigdata*))
  (loop for (x tp) in data
	for fit-tp = (funcall fit x)
	maximizing (abs (* 100 (/ (- tp fit-tp) fit-tp)))))

(defun max-fit-%diff (func initial &optional (data *bigdata*))
  (multiple-value-bind (params r^2) (fit::fit func data initial t nil)
    (table 6
      (row "max %diff" "R^2" "Params")
      (row (loop for (x tp) in data
		 for fit-tp = (apply func x params)
		 maximizing (abs (* 100 (/ (- tp fit-tp) fit-tp))))
	    r^2
	    params))))

(defun map-data (func &optional (data *bigdata*))
  (loop for (x tp) in data do
    (funcall func x tp)))

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
