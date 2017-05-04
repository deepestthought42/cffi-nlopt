(in-package #:cffi-nlopt)




(defgeneric no-dimensions (obj)
  (:documentation "Return the no of dimensions (independent variables)
  of the model to optimize"))

(defgeneric function-to-optimize (obj)
  (:documentation "Return a closure of type (FUNCTION ((CFFI:ARRAY
  DOUBLE-FLOAT) DOUBLE-FLOAT) to optimize."))

(defgeneric upper-bounds (obj)
  (:documentation "Return a SEQUENCE of upper bounds for OBJ."))

(defgeneric lower-bounds (obj)
  (:documentation "Retrun a SEQUENCE of lower bounds for the paramters
  to be optimized."))

(defgeneric initial-guess (obj)
  (:documentation "Return a (SIMPLE-ARRAY DOUBLE-FLOAT) of initial
  guesses for the parameters to be optimized."))



(defclass config ()
  ((optimization-type :accessor optimization-type :initarg :optimization-type
		      :initform :minimization)
   (algorithm :accessor algorithm :initarg :algorithm :initform +nlopt_ln_neldermead+)
   (stop-val :accessor stop-val :initarg :stop-val :initform nil)
   (f-abs-tolerance :accessor f-abs-tolerance :initarg :f-abs-tolerance :initform 1d-6)
   (f-rel-tolerance :accessor f-rel-tolerance :initarg :f-rel-tolerance :initform 1d-6)
   (x-abs-tolerance :accessor x-abs-tolerance :initarg :x-abs-tolerance :initform nil)
   (x-rel-tolerance :accessor x-rel-tolerance :initarg :x-rel-tolerance :initform nil)
   (max-no-evaluations :accessor max-no-evaluations :initarg :max-no-evaluations :initform nil)
   (max-time :accessor max-time :initarg :max-time :initform 1d0)))


(defmacro %with-double-arrays ((n &rest sequences) &body body)
  (let ((tmp-seqs (iter
		    (for s in sequences)
		    (collect (gensym (string (alexandria:symbolicate 'tmp/ s)))))))
    `(let (,@(iter
	       (for tmp/s in tmp-seqs)
	       (for s in sequences)
	       (collect (list tmp/s s))))
       (cffi:with-foreign-objects (,@(iter
				       (for s in sequences)
				       (collect `(,s :double ,n))))
	 (iter
	   (for s in (list ,@sequences))
	   (for ts in (list ,@tmp-seqs))
	   (iter
	     (for i from 0 below ,n)
	     (setf (cffi:mem-aref s :double i)
		   (elt ts i))))
	 (progn ,@body)))))

(defun set-stopping-conditions (opt config)
  (let+ (((&slots f-abs-tolerance f-rel-tolerance
		  x-abs-tolerance x-rel-tolerance
		  max-no-evaluations max-time) config))
    (iter
      (for val in (list f-abs-tolerance f-rel-tolerance
			x-abs-tolerance x-rel-tolerance
			max-no-evaluations max-time))
      (for fun in '(%set-ftol-abs %set-ftol-rel
		    %set-xtol-abs %set-xtol-rel
		    %set-maxeval %set-maxtime))
      (if val
	  (check/nlopt-result (funcall fun opt val))))))

(defun set-bounds (upper-bounds lower-bounds opt)
  (check/nlopt-result
   (%set-upper-bounds opt upper-bounds))
  (check/nlopt-result
   (%set-lower-bounds opt lower-bounds)))




(defun optimization (model config)
  (let+ (((&slots optimization-type algorithm) config)
	 (n (no-dimensions model))
	 (initial-guess (initial-guess model))
	 (upper-bounds (upper-bounds model))
	 (lower-bounds (lower-bounds model))
	 (*optimization-func* (function-to-optimize model)))
    (%with-double-arrays (n upper-bounds lower-bounds initial-guess)
      (cffi:with-foreign-object (optimized-val :double)
	(with-nlopt-algorithm (opt algorithm n)
	  (set-bounds upper-bounds lower-bounds opt)
	  (set-stopping-conditions opt config)
	  (case optimization-type
	    ((:minimize :min)
	     (check/nlopt-result
	      (%set-min-objective opt (cffi:callback optimization-func) (cffi-sys:null-pointer))))
	    ((:maximize :max)
	     (check/nlopt-result
	      (%set-max-objective opt (cffi:callback optimization-func) (cffi-sys:null-pointer)))))
	  (check/nlopt-result
	   (%optimize opt initial-guess optimized-val)))))))

