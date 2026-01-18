(load "package.lisp")
(load "cl-clojure-syntax.lisp")
(load "cl-clojure-eval.lisp")
(load "cl-clojure-transducers.lisp")

(use-package :cl-clojure-syntax)
(use-package :cl-clojure-eval)

;; Initialize the eval system
(init-eval-system)

;; Test range
(format t "~&=== Testing range ===~%")
(let ((path (make-pathname :name "test_range" :type "clj"
                           :defaults (make-pathname :directory '(:relative ".")))))
  (with-open-file (s path :direction :input)
    (let ((*readtable* (ensure-clojure-readtable)))
      (loop for form = (read-clojure s nil :eof)
            until (eq form :eof)
            do (let ((result (clojure-eval form *current-env*)))
                 (format t "Result type: ~a~%" (type-of result))
                 (format t "Result: ~a~%" result)
                 ;; Check if it's a lazy range
                 (when (lazy-range-p result)
                   (format t "  Lazy range start=~a end=~a step=~a~%"
                           (lazy-range-start result)
                           (lazy-range-end result)
                           (lazy-range-step result))))))))
