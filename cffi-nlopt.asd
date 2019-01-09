;;;; cffi-nlopt.asd

(asdf:defsystem #:cffi-nlopt
  :description "NLOpt foreign function interface for use in bayesian-analysis"
  :author "Renee Klawitter <klawitterrenee@gmail.com>"
  :license "Apache 2.0"
  :depends-on (#:cffi
               #:alexandria
               #:let-plus)
  :serial t
  :components ((:file "package")
	       (:file "constants")
               (:file "cffi-nlopt")
	       (:file "api")))

