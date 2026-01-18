(load "package.lisp")
(load "cl-clojure-syntax.lisp")
(load "cl-clojure-eval.lisp")
(load "cl-clojure-transducers.lisp")

(use-package :cl-clojure-syntax)
(use-package :cl-clojure-eval)

;; Initialize the eval system
(init-eval-system)

;; Function to test a single Clojure file
(defun test-clojure-file (filename)
  (let ((path (merge-pathnames (make-pathname :name filename :type "clj")
                               (make-pathname :directory '(:relative ".")))))
    (format t "~&=== Testing ~a ===~%" filename)
    (with-open-file (s path :direction :input)
      (let ((*readtable* (ensure-clojure-readtable)))
        (loop for form = (read-clojure s nil :eof)
              until (eq form :eof)
              do (let ((result (clojure-eval form *current-env*)))
                   (format t "Result: ~a~%" result)))))))

;; Test each file
(test-clojure-file "test_for_simple")
(test-clojure-file "test_for_let")
(test-clojure-file "test_for_let_when")
