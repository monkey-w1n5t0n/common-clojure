(asdf:defsystem #:cl-clojure
  :description "Clojure on SBCL - Reader and Evaluator"
  :author "Droid"
  :license "MIT"
  :serial t
  :depends-on ()
  :components ((:file "package")
               (:file "cl-clojure-syntax")
               (:file "cl-clojure-eval"))
  :around-compile (lambda (thunk)
                    (handler-bind ((warning #'muffle-warning))
                      (funcall thunk))))
