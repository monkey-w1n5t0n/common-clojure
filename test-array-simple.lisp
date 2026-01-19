#!/usr/bin/env sbcl --script

(load "package.lisp")
(load "cl-clojure-syntax.lisp")
(load "cl-clojure-transducers.lisp")
(load "cl-clojure-eval.lisp")

(in-package :cl-clojure-eval)

(init-eval-system)

;; Try evaluating array_symbols forms
(format t "Testing array_symbols...~%")

(handler-case
    (progn
      (format t "Loading clojure-tests/array_symbols.clj...~%")
      (let ((results (eval-file "clojure-tests/array_symbols.clj")))
        (format t "Success! Results: ~A~%" results)))
  (error (c)
    (format t "Error: ~A~%" c)
    (format t "Type: ~A~%" (type-of c))
    (sb-debug:backtrace)))
