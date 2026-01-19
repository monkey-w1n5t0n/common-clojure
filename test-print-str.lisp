(load "package.lisp")
(load "cl-clojure-syntax.lisp")
(load "cl-clojure-transducers.lisp")
(load "cl-clojure-eval.lisp")

(in-package :cl-clojure-eval)

(init-eval-system)

;; Test clojure-print-str directly
(format t "Testing clojure-print-str with symbol...~%")
(let ((result (clojure-print-str 'long/1)))
  (format t "(clojure-print-str 'long/1) = ~A~%" result))

;; Test with symbol that has /
(format t "~&Testing with interned symbol...~%")
(let ((sym (intern "LONG/1" :cl-clojure-eval)))
  (format t "Symbol: ~A~%" sym)
  (let ((result (clojure-print-str sym)))
    (format t "(clojure-print-str sym) = ~A~%" result)))
