;;;; cffi-nlopt.asd

(asdf:defsystem #:cffi-nlopt
  :description "Describe cffi-nlopt here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cffi
               #:alexandria
               #:let-plus)
  :serial t
  :components ((:file "package")
	       (:file "constants")
               (:file "cffi-nlopt")
	       (:file "api")))

