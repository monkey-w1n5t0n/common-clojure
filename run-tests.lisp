;; Run tests - load with sbcl --load run-tests.lisp
;; Or use ./tests.sh

;; Load strict compiler settings first
(load "sbcl-init.lisp")

(load "package.lisp")
(load "cl-clojure-syntax.lisp")
(load "cl-clojure-eval.lisp")
(load "cl-clojure-transducers.lisp")

(defpackage #:clojure-test-runner
  (:use #:cl #:cl-clojure-syntax #:cl-clojure-eval))

(in-package #:clojure-test-runner)

(defun get-test-dir ()
  (let* ((this-file *load-truename*)
         (this-dir (make-pathname :directory (pathname-directory this-file)))
         (test-dir (merge-pathnames (make-pathname :directory '(:relative "clojure-tests"))
                                     this-dir)))
    test-dir))

(defvar *test-dir* (get-test-dir))

(defun read-file-to-string (path)
  (with-open-file (s path :direction :input)
    (let ((content (make-string (file-length s))))
      (read-sequence content s)
      content)))

(defun try-run-clojure-file (path)
  (handler-case
      (progn
        (unless *current-env*
          (init-eval-system))
        (eval-file (namestring path))
        (values :ok (pathname-name path)))
    (serious-condition (c)
      (declare (ignore c))
      (values :error (pathname-name path)))))

(let ((files (directory (merge-pathnames "*.clj" *test-dir*)))
      (passed nil)
      (failed nil))
  (dolist (file files)
    (multiple-value-bind (status name)
        (try-run-clojure-file file)
      (if (eq status :ok)
          (push name passed)
          (push name failed))))
  (setf passed (nreverse passed))
  (setf failed (nreverse failed))
  (format t "~&=== Passed Tests ===~%")
  (dolist (name passed)
    (format t "  ~A~%" name))
  (format t "~&=== Failed Tests ===~%")
  (dolist (name failed)
    (format t "  ~A~%" name))
  (format t "~&Total: ~D passed, ~D failed~%" (length passed) (length failed))
  ;; Exit with proper status code for CI/completion detection
  ;; Exit 0 only if ALL tests pass, exit 1 if any failed
  (sb-ext:exit :code (if failed 1 0)))
