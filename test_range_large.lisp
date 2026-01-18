(load "package.lisp")
(load "cl-clojure-syntax.lisp")
(load "cl-clojure-eval.lisp")
(load "cl-clojure-transducers.lisp")

(in-package :cl-clojure-eval)

;; Initialize the eval system
(init-eval-system)

;; Test range with large number
(format t "~&=== Testing range with large number ===~%")
(let ((code "(range 100000000)"))
  (format t "Testing: ~a~%" code)
  (let ((result (cl-clojure-syntax:read-clojure-string code)))
    (format t "Parsed: ~a~%" result)
    (let ((eval-result (clojure-eval result *current-env*)))
      (format t "Result type: ~a~%" (type-of eval-result))
      (when (cl-clojure-eval::lazy-range-p eval-result)
        (format t "  Lazy range start=~a end=~a step=~a~%"
                (cl-clojure-eval::lazy-range-start eval-result)
                (cl-clojure-eval::lazy-range-end eval-result)
                (cl-clojure-eval::lazy-range-step eval-result)))))))
