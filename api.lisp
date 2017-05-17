(in-package #:cffi-nlopt)




(defgeneric no-dimensions (model)
  (:documentation "Return the no of dimensions (independent variables)
  of the model to optimize"))

(defgeneric function-to-optimize (model)
  (:documentation "Return a closure of type (FUNCTION ((CFFI:ARRAY
  DOUBLE-FLOAT) DOUBLE-FLOAT) to that represents to function to optimize MODEL with."))

(defgeneric upper-bounds (model)
  (:documentation "Return a SEQUENCE of upper bounds for MODEL."))

(defgeneric lower-bounds (model)
  (:documentation "Return a SEQUENCE of lower bounds for the paramters
  of MODEL to be optimized."))

(defgeneric initial-guess (model)
  (:documentation "Return a (SIMPLE-ARRAY DOUBLE-FLOAT) of initial
  guesses for the parameters of MODEL to be optimized."))



(defclass config ()
  ((optimization-type :accessor optimization-type :initarg :optimization-type
		      :initform :maximize)
   (algorithm :accessor algorithm :initarg :algorithm :initform +nlopt_ln_neldermead+)
   (stop-val :accessor stop-val :initarg :stop-val :initform nil)
   (f-abs-tolerance :accessor f-abs-tolerance :initarg :f-abs-tolerance :initform nil)
   (f-rel-tolerance :accessor f-rel-tolerance :initarg :f-rel-tolerance :initform 1d-6)
   (x-abs-tolerance :accessor x-abs-tolerance :initarg :x-abs-tolerance :initform nil)
   (x-rel-tolerance :accessor x-rel-tolerance :initarg :x-rel-tolerance :initform 1d-6)
   (max-no-evaluations :accessor max-no-evaluations :initarg :max-no-evaluations :initform nil)
   (max-time :accessor max-time :initarg :max-time :initform 1d0)))


(defmacro %with-double-arrays ((n &rest sequences) &body body)
  "Macro that 'converts' the sequences in SEQUENCES to
cffi:array and binds them to variables of the same name around BODY."
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

(defun set-bounds (opt upper-bounds lower-bounds)
  (check/nlopt-result
   (%set-upper-bounds opt upper-bounds))
  (check/nlopt-result
   (%set-lower-bounds opt lower-bounds)))

(defun set-objective (opt optimization-type)
  (check/nlopt-result
   (funcall
    (case optimization-type
      ((:minimize :min :minimization) #'%set-min-objective)
      ((:maximize :max :maximization) #'%set-max-objective))
    opt
    (cffi:callback optimization-func)
    (cffi-sys:null-pointer))))


(defun optimization (model config)
  "Optimize model MODEL according to config given in CONFIG.  Here
CONFIG is of type cffi-nlopt:config and MODEL must implement the
following generic functions:

nlopt:no-dimensions,
nlopt:function-to-optimize,
nlopt:upper-bounds,
nlopt:lower-bounds,
nlopt:initial-guess

"
  (let+ (((&slots optimization-type algorithm) config)
	 (n (no-dimensions model))
	 (initial-guess (initial-guess model))
	 (upper-bounds (upper-bounds model))
	 (lower-bounds (lower-bounds model))
	 (*optimization-func* (function-to-optimize model)))
    (%with-double-arrays (n upper-bounds lower-bounds initial-guess)
      (cffi:with-foreign-object (optimized-val :double)
	(with-algorithm (opt algorithm n)
	  (set-bounds opt upper-bounds lower-bounds)
	  (set-stopping-conditions opt config)
	  (set-objective opt optimization-type)
	  (let ((retval (%optimize opt initial-guess optimized-val)))
	    (funcall *optimization-func* initial-guess)
	    (values retval model (cffi:mem-ref optimized-val :double))))))))
