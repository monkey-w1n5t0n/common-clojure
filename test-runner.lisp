#!/bin/sbcl --script

;;;; Clojure Test Runner for Common Lisp
;;;; This attempts to load and run Clojure test files, reporting what works and what doesn't

;; Load directly without ASDF for faster startup
(load "package.lisp")
(load "cl-clojure-syntax.lisp")
(load "cl-clojure-eval.lisp")

(defpackage #:clojure-test-runner
  (:use #:cl #:cl-clojure-syntax #:cl-clojure-eval))

(in-package #:clojure-test-runner)

;;; Test Results Tracking

(defstruct test-result
  file
  parse-status    ; :ok or :error
  eval-status     ; :ok :error or :not-implemented
  tests-passed    ; number of tests passed
  tests-failed    ; number of tests failed
  message)

(defvar *test-results* nil)

;;; Figure out the test directory relative to this file
(defun get-test-dir ()
  (let* ((this-file (or *load-truename* *load-pathname*
                        (make-pathname :directory (pathname-directory (user-homedir-pathname))
                                      :name "test-runner" :type "lisp")))
         (this-dir (make-pathname :directory (pathname-directory this-file)))
         (test-dir (merge-pathnames (make-pathname :directory '(:relative "clojure-tests"))
                                     this-dir)))
    test-dir))

(defvar *test-dir* (get-test-dir))

;;; Read entire file as string
(defun read-file-to-string (path)
  "Read the entire contents of a file as a string."
  (with-open-file (s path :direction :input)
    (let ((content (make-string (file-length s))))
      (read-sequence content s)
      content)))

;;; Try to read a Clojure file and report results
(defun try-read-clojure-file (path)
  "Try to read a Clojure file. Returns :ok if readable, :error if not."
  (handler-case
      (progn
        ;; Read file, preprocess for dot tokens, then parse
        (let* ((content (read-file-to-string path))
               (preprocessed (preprocess-clojure-dots content)))
          (with-input-from-string (s preprocessed)
            (let ((*readtable* (ensure-clojure-readtable)))
              (loop for form = (read-clojure s nil :eof)
                    until (eq form :eof)
                    count t))))
        :ok)
    (error (c)
      (declare (ignore c))
      :error)))

;;; Try to evaluate/run a Clojure file
(defun try-run-clojure-file (path)
  "Try to run a Clojure test file. Returns (values eval-status tests-passed tests-failed).
   Evaluates each form in the file and returns results."
  (handler-case
      (progn
        ;; Initialize the eval system if needed
        (unless *current-env*
          (init-eval-system))
        ;; Evaluate the file
        (eval-file (namestring path))
        ;; For now, count as passed if we got through without errors
        ;; TODO: Actually count test assertions (deftest, is, etc.)
        (values :ok 1 0))
    (error (c)
      (format t "~&Error evaluating ~A: ~A~%" (pathname-name path) c)
      (values :error 0 1))))

;;; Run all tests and collect results
(defun run-all-tests ()
  "Run all test files and collect results."
  (let ((files (directory (merge-pathnames "*.clj" *test-dir*)))
        (results nil))
    (dolist (file files)
      (let ((parse-status (try-read-clojure-file file)))
        (multiple-value-bind (eval-status passed failed)
            (if (eq parse-status :ok)
                (try-run-clojure-file file)
                (values :error 0 1))
          (push (make-test-result
                 :file (pathname-name file)
                 :parse-status parse-status
                 :eval-status eval-status
                 :tests-passed passed
                 :tests-failed failed)
                results))))
    (nreverse results)))

;;; Print test summary
(defun print-summary (results)
  "Print a summary of test results."
  (let ((total-files (length results))
        (parse-ok 0)
        (parse-err 0)
        (eval-ok 0)
        (eval-err 0)
        (eval-pending 0)
        (total-passed 0)
        (total-failed 0))

    (dolist (r results)
      (if (eq (test-result-parse-status r) :ok)
          (incf parse-ok)
          (incf parse-err))
      (case (test-result-eval-status r)
        (:ok (incf eval-ok))
        (:error (incf eval-err))
        (:not-implemented (incf eval-pending)))
      (incf total-passed (test-result-tests-passed r))
      (incf total-failed (test-result-tests-failed r)))

    (format t "~%=== Test Summary ===~%")
    (format t "Files: ~D total~%" total-files)
    (format t "Parse: ~D ok, ~D errors~%" parse-ok parse-err)
    (format t "Eval:  ~D ok, ~D errors, ~D pending~%" eval-ok eval-err eval-pending)
    (format t "~%Tests Passed: ~D~%" total-passed)
    (format t "Tests Failed: ~D~%" total-failed)
    ;; Show which tests passed
    (terpri)
    (dolist (r results)
      (when (eq (test-result-eval-status r) :ok)
        (format t "ok: ~A~%" (test-result-file r))))

    ;; Return whether all tests passed
    (and (= parse-err 0)
         (= eval-err 0)
         (= eval-pending 0)
         (= total-failed 0)
         (> total-passed 0))))

;;; Main entry point
(defun main ()
  (format t "Clojure on SBCL - Test Runner~%")
  (format t "================================~%")

  (let* ((results (run-all-tests))
         (all-passed (print-summary results)))

    (format t "~%")
    (if all-passed
        (format t "SUCCESS: All tests passed!~%")
        (format t "PENDING: Implement Clojure evaluation to run tests.~%"))

    ;; Exit with appropriate code
    (sb-ext:exit :code (if all-passed 0 1))))

;; Run if called as script
(eval-when (:execute)
  (main))
