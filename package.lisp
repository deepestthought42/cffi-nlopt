;;;; package.lisp

(defpackage #:cffi-nlopt
  (:use #:cl #:let-plus #:iterate)
  (:export
   #:no-dimensions
   #:initial-guess
   #:lower-bounds
   #:upper-bounds
   #:function-to-optimize
   #:config
   #:optimization-type
   #:algorithm
   #:stop-val
   #:f-abs-tolerance
   #:f-rel-tolerance
   #:x-abs-tolerance
   #:x-rel-tolerance
   #:max-no-evaluations
   #:max-time)
  (:nicknames #:nlopt))

