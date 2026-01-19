;;;; -*- Mode: Lisp -*-

(load "package.lisp")
(load "cl-clojure-syntax.lisp")
(load "cl-clojure-eval.lisp")

(in-package :cl-clojure-eval)

(cl-clojure-syntax:ensure-clojure-readtable)

;; Test the :or destructuring
(let ((*readtable* cl-clojure-syntax:*clojure-readtable*))
  (format t "Testing :or destructuring...~%")
  (handler-case
      (let ((result (eval-string "(let [{:keys [a] :or {b 2}} {:a 1}] [a b])")))
        (format t "Result: ~a (should have thrown)~%" result))
    (error (c)
      (format t "Caught error: ~a~%" c))))

;; Test the actual keywords-in-destructuring
(format t "~%Testing keywords-in-destructuring...~%")
(handler-case
    (let ((result (eval-string "(let [m {:a 1 :b 2}] (let [{:keys [:a :b]} m] [a b]))")))
      (format t "Result: ~a~%" result))
  (error (c)
    (format t "Caught error: ~a~%" c)))
