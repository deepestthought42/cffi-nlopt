;;;; cffi-nlopt.lisp

(in-package #:cffi-nlopt)

;;; "cffi-nlopt" goes here. Hacks and glory await!



(cffi:define-foreign-library libnlopt
  (:unix (:or "libnlopt.so")))



(cffi:use-foreign-library libnlopt)



;;; versioning


(cffi:defcfun ("nlopt_version" %nlopt-version) :void
  (major (:pointer :int))
  (minor (:pointer :int))
  (bugfix (:pointer :int)))


(defun nlopt-version ()
  "Returns nlopt version as 
=> (VALUES MAJOR MINOR BUGFIX)"
  (cffi:with-foreign-objects ((major :int)
			      (minor :int)
			      (bugfix :int))
    (nlopt_version major minor bugfix)
    (values
     (cffi:mem-ref major :int)
     (cffi:mem-ref minor :int)
     (cffi:mem-ref bugfix :int))))

;;; algorithm name

(cffi:defcfun ("nlopt_algorithm_name" nlopt-algorithm-name)
    :string
  (a nlopt-algorithm))

;;; nlopt_opt objects

(cffi:defctype nlopt-opt :pointer)

(cffi:defcfun ("nlopt_create" %create-opt-object)
    nlopt-opt
  (algorithm nlopt-algorithm)
  (n :unsigned-int))

(cffi:defcfun ("nlopt_copy" %copy-opt-object)
    nlopt-opt
  (opt nlopt-opt))


(cffi:defcfun ("nlopt_destroy" %destroy-opt-object)
    :void
  (opt nlopt-opt))




(defmacro with-algorithm ((object-name algorithm n) &body body)
  `(progn
     (check-type ,n integer)
     (let ((,object-name (%create-opt-object ,algorithm ,n)))
       (unwind-protect
	    (progn ,@body)
	 (%destroy-opt-object ,object-name)))))




;; optimization 

(cffi:defctype nlopt-func :pointer)

(cffi:defcfun ("nlopt_optimize" %optimize)
    nlopt-result
  (opt nlopt-opt)
  (x (:pointer :double))
  (opt-f :pointer))

(cffi:defcfun ("nlopt_set_min_objective" %set-min-objective)
    nlopt-result
  (opt nlopt-opt)
  (f nlopt-func)
  (f-data (:pointer :void)))

(cffi:defcfun ("nlopt_set_max_objective" %set-max-objective)
    nlopt-result
  (opt nlopt-opt)
  (f nlopt-func)
  (f-data (:pointer :void)))


;; stopping criteria

(cffi:defcfun ("nlopt_set_stopval" %set-stopval)
    nlopt-result
  (opt nlopt-opt)
  (stopval :double))

(cffi:defcfun ("nlopt_set_ftol_rel" %set-ftol-rel)
    nlopt-result
  (opt nlopt-opt)
  (tol :double))

(cffi:defcfun ("nlopt_set_ftol_abs" %set-ftol-abs)
    nlopt-result
  (opt nlopt-opt)
  (tol :double))

(cffi:defcfun ("nlopt_set_xtol_rel" %set-xtol-rel)
    nlopt-result
  (opt nlopt-opt)
  (tol :double))

(cffi:defcfun ("nlopt_set_xtol_abs1" %set-xtol-abs1)
    nlopt-result
  (opt nlopt-opt)
  (tol :double))

(cffi:defcfun ("nlopt_set_xtol_abs" %set-xtol-abs)
    nlopt-opt
  (opt nlopt-opt)
  (tol (:pointer :double)))

(cffi:defcfun ("nlopt_set_maxeval" %set-maxeval)
    nlopt-result
  (opt nlopt-opt)
  (maxeval :int))


(cffi:defcfun ("nlopt_set_maxtime" %set-maxtime)
    nlopt-result
  (opt nlopt-opt)
  (maxtime :double))

(cffi:defcfun ("nlopt_force_stop" %set-force-stop)
    nlopt-result
  (opt nlopt-opt))





;; bounds

(cffi:defcfun ("nlopt_set_upper_bounds" %set-upper-bounds)
    nlopt-result
  (opt nlopt-opt)
  (up (:pointer :double)))

(cffi:defcfun ("nlopt_set_lower_bounds" %set-lower-bounds)
    nlopt-result
  (opt nlopt-opt)
  (lb (:pointer :double)))


;;; callbacks

(defparameter *optimization-func* #'(lambda (xa)
				      (declare (ignore xa)) 0d0))

(cffi:defcallback optimization-func :double
    ((n :unsigned-int)
     (x (:pointer :double))
     (gradient (:pointer :double))
     (func-data (:pointer :void)))
  (declare (ignore n gradient func-data))
  (funcall *optimization-func* x))
