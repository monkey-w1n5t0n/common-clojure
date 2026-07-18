;;;; Diagnostic script to analyze failing Clojure test files
;;;; Usage: sbcl --noinform --disable-debugger --load diagnose-failures.lisp
;;;; IMPORTANT: This file must NOT use [] or {} syntax

;; Load system first - this installs the Clojure readtable
(load "sbcl-init.lisp")
(load "package.lisp")
(load "cl-clojure-syntax.lisp")

;; Save the standard readtable and use it for our file
(defparameter *standard-rt* (copy-readtable nil))

;; Restore standard readtable so we can load our file
(defun load-with-standard-readtable (path)
  (let ((*readtable* *standard-rt*))
    (load path)))

;; Now load eval system with standard readtable
(let ((*readtable* *standard-rt*))
  (load "cl-clojure-eval.lisp")
  (load "cl-clojure-transducers.lisp"))

;; Now switch to cl-clojure-eval package for our work
(cl-clojure-eval::%run-diagnostics)
