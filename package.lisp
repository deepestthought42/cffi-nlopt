;;;; package.lisp

(defpackage #:cffi-nlopt
  (:use #:cl #:let-plus #:iterate)
  (:export
   #:no-dimensions
   #:initial-guess
   #:lower-bounds
   #:upper-bounds
   #:function-to-optimize))

