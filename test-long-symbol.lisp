(load "package.lisp")
(load "cl-clojure-syntax.lisp")
(load "cl-clojure-transducers.lisp")
(load "cl-clojure-eval.lisp")

(in-package :cl-clojure-eval)

(init-eval-system)

(clojure-eval '(ns clojure.test-clojure.array-symbols
                 (use clojure.test)
                 (require [clojure.test-helper :as util])) *current-env*)

(format t "Testing long/1 symbol...~%")
(let ((result (clojure-eval 'long/1 *current-env*)))
  (format t "long/1 evaluates to: ~A~%" result)
  (format t "Type: ~A~%" (type-of result)))

(format t "~&Testing print-str...~%")
(let ((result (clojure-eval '(print-str long/1) *current-env*)))
  (format t "(print-str long/1) evaluates to: ~A~%" result))

(format t "~&Testing = comparison...~%")
(let ((result (clojure-eval '(= "long/1" (print-str long/1)) *current-env*)))
  (format t "(= \"long/1\" (print-str long/1)) evaluates to: ~A~%" result))
