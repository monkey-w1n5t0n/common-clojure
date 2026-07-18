;; Helper to diagnose a single test file
;; Usage: sbcl --noinform --disable-debugger --non-interactive --load diagnose-one.lisp --eval '(diagnose "test_name")'
;; The test name is passed as the first argument

(load "sbcl-init.lisp")
(load "package.lisp")
(load "cl-clojure-syntax.lisp")
(load "cl-clojure-eval.lisp")
(load "cl-clojure-transducers.lisp")

;; Define in CL-USER package to avoid package issues
;; But use cl-clojure-eval symbols

(defun diagnose (test-name)
  (let ((path (make-pathname :name test-name :type "clj"
               :defaults (merge-pathnames
                           (make-pathname :directory '(:relative "clojure-tests"))
                           (make-pathname :directory (pathname-directory *load-truename*))))))
    (setf cl-clojure-eval::*current-env* nil)
    (handler-case
      (progn
        (cl-clojure-eval::init-eval-system)
        (let* ((content (with-open-file (s path :direction :input)
                          (let ((str (make-string (file-length s))))
                            (read-sequence str s) str)))
               (preprocessed (cl-clojure-syntax:preprocess-clojure-dots content))
               (comment-marker (cl-clojure-syntax:get-comment-marker))
               (form-count 0))
          (with-input-from-string (stream preprocessed)
            (let ((*readtable* (cl-clojure-syntax:ensure-clojure-readtable)))
              (loop for form = (cl-clojure-syntax:read-clojure stream nil :eof)
                    until (eq form :eof)
                    do (unless (eq form comment-marker)
                         (incf form-count)
                         (handler-case
                           (cl-clojure-eval::clojure-eval form cl-clojure-eval::*current-env*)
                           (error (c)
                             (format t "EVAL-ERROR at form #~D: ~A~%" form-count c)
                             (return-from diagnose nil))))))))
        (format t "PASSED~%"))
      (cl:reader-error (c) (format t "READER-ERROR: ~A~%" c))
      (cl:stream-error (c) (format t "STREAM-ERROR: ~A~%" c))
      (cl:end-of-file (c) (format t "EOF-ERROR: ~A~%" c))
      (cl:type-error (c) (format t "TYPE-ERROR: ~A~%" c))
      (cl:error (c) (format t "ERROR: ~A~%" c)))))
