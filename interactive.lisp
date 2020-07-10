(defpackage :doing
  (:use :cl)
  (:local-nicknames (:m :moi)
		    (:gui :sdl-gui-utils)))

(in-package :doing)



(defun test-bay-animate ()
  (let ((evh (make-instance 'gui::events-handler))
	(height 12)
	(bays-x 4)
	(bays-y 4)
	l b
	(n 4)
	data
	plotter
	button
	slider)
    (labels ((calculate ()
	       (let ((bg (moi::make-building-geometry :number-of-storey n
						      :l l :b b :h height))
		     (sg (moi::make-structural-geometry :beam-height 0.5d0)))
		 (setf data nil)
		 (loop for bay-size from 5 to 50 by 0.2 do
		   (setf (moi::bg-l bg) (make-array bays-x :initial-element bay-size))
		   (setf (moi::bg-b bg) (make-array bays-y :initial-element bay-size))
		   (push (list bay-size (moi::fundamental-time-period bg sg)) data))
		 (gui::add-frame plotter height data)))			   
	     (update ()
	       (setf (slot-value plotter 'gui::origin-x) 1.0
		     (slot-value plotter 'gui::origin-y) nil)
	       (calculate)))
      ;; Height
      (setf slider (gui::push-interface evh (make-instance 'gui::slider-with-entry&name
							   :name "Height"
							   :value height
							   :range #(1 30)
							   :x 100 :y 140 :height 20 :slider-width 100
							   :callback (lambda (w)
								       (setf height (gui::value w))
								       (update)
								       (gui::calculate-clipped-data plotter)))
					:hoverable? nil))
      
      (setf plotter (gui::push-interface evh (make-instance 'gui::animation-plotter
							    :fulldata nil
							    :origin-x 1.0
							    :origin-y nil
							    :x 50
							    :y 200
							    :width 600
							    :height 500)))
      (calculate)
      (setf button (gui::push-interface evh (make-instance 'gui::button
							   :x 50 :y 100
							   )))
      (gui::loop-with-events-to (evh)
				(if (slot-value button 'gui::state)
				    (setf (gui::value slider) (gui::next-frame plotter)))
				nil))))




(defun test-height ()
  (let ((evh (make-instance 'gui::events-handler))
	(height 5)
	(bays-x 1)
	(bays-y 1)
	l b
	(bay-size 10)
	(n 10)
	data
	plotter)
    (labels ((calculate ()
	       (setf l (make-array bays-x :initial-element bay-size))
	       (setf b (make-array bays-y :initial-element bay-size))
	       (let ((bg (moi::make-building-geometry :number-of-storey n
						      :l l :b b :h height))
		     (sg (moi::make-structural-geometry :beam-height 0.1d0)))
		 (setf data nil)
		 (loop for h from 0.11 to height by 0.5 do
		   (setf (moi::bg-h bg) h)
		   (push (list h (moi::fundamental-time-period bg sg)) data))))
	     (update ()
	       (calculate)
	       (setf (slot-value plotter 'gui::origin-x) 0.0
		     (slot-value plotter 'gui::origin-y) 0.0)
	       (setf (gui::data plotter) data)))			 
      ;; height
      (gui::push-interface evh (make-instance 'gui::slider-with-entry&name
					      :name "Height"
					      :value height
					      :x 100 :y 50 :height 20 :slider-width 100
					      :callback (lambda (w)
							  (setf height (gui::value w))
							  (update)))
			   :hoverable? nil)
      ;; n 
      (gui::push-interface evh (make-instance 'gui::slider-with-entry&name
					      :name "N"
					      :value n
					      :range #(1 20)
					      :x 100 :y 80 :height 20 :slider-width 100
					      :callback (lambda (w)
							  (setf n (truncate (gui::value w)))
							  (setf (gui::value w) n)
							  (update)))
			   :hoverable? nil)

      ;; bays-x
      (gui::push-interface evh (make-instance 'gui::slider-with-entry&name
					      :name "Bays-x"
					      :value bays-x
					      :range #(1 20)
					      :x 100 :y 110 :height 20 :slider-width 100
					      :callback (lambda (w)
							  (setf bays-x (truncate (gui::value w)))
							  (setf (gui::value w) bays-x)
							  (update)))
			   :hoverable? nil)

      
      ;; bays-size
      (gui::push-interface evh (make-instance 'gui::slider-with-entry&name
					      :name "Bay size"
					      :value bay-size
					      :range #(1 30)
					      :x 100 :y 140 :height 20 :slider-width 100
					      :callback (lambda (w)
							  (setf bay-size (gui::value w))
							  (update)))
			   :hoverable? nil)
      
      (calculate)
      (setf plotter (gui::push-interface evh (make-instance 'gui::plotter
							    :data data
							    :origin-x 0.0
							    :x 50
							    :y 200
							    :width 600
							    :height 500)))
      (print data)
      (gui::loop-with-events-to (evh)
				nil))))









(defun test-plot ()
  (let ((evh (make-instance 'gui::events-handler)))
    (gui::push-interface evh (make-instance 'gui::plotter
					    :x 100
					    :y 100
					    :width 200
					    :height 200
					    :data '((1 1) (2 2) (3 5) (4 2) (5 8))))
    (gui::loop-with-events-to (evh)
			      nil)))

(defun test1 ()
  (let ((evh (make-instance 'gui::events-handler)))
    (gui::push-interface evh (make-instance 'gui::slider-with-entry
					    :x 50
					    :y 100
					    :height 20
					    :slider-width 100) :hoverable? nil)
    (gui::push-interface evh (make-instance 'gui::slider-with-entry
					    :x 50
					    :y 150
					    :height 20
					    :slider-width 100) :hoverable? nil)
    (gui::loop-with-events-to (evh)
			      nil)))
