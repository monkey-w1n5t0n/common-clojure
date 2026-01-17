#!/bin/sbcl --script

;;;; Clojure Test Runner for Common Lisp
;;;; This attempts to load and run Clojure test files, reporting what works and what doesn't

(require 'asdf)
(load "cl-clojure-syntax.asd")
(asdf:load-system :cl-clojure-syntax)
(load "cl-clojure.asd")
(asdf:load-system :cl-clojure)

(defpackage #:clojure-test-runner
  (:use #:cl #:cl-clojure-syntax)
  (:import-from #:cl-clojure-eval
                #:init-eval-system
                #:eval-file
                #:reset-env))

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
  "Try to run a Clojure test file. Returns (values eval-status tests-passed tests-failed)."
  (handler-case
      (progn
        ;; Reset environment for each test file
        (reset-env)
        ;; Evaluate all forms in the file
        (eval-file path)
        ;; For now, consider it "ok" if we can evaluate without errors
        ;; TODO: Actually track test assertions
        (values :ok 0 0))
    (error (c)
      ;; Return error status with error message
      (values :error 0 1 (format nil "~A" c)))))

;;; Run all tests and collect results
(defun run-all-tests ()
  "Run all test files and collect results."
  (let ((files (directory (merge-pathnames "*.clj" *test-dir*)))
        (results nil))
    (dolist (file files)
      (let ((parse-status (try-read-clojure-file file)))
        (multiple-value-bind (eval-status passed failed message)
            (if (eq parse-status :ok)
                (try-run-clojure-file file)
                (values :error 0 1 nil))
          (push (make-test-result
                 :file (pathname-name file)
                 :parse-status parse-status
                 :eval-status eval-status
                 :tests-passed passed
                 :tests-failed failed
                 :message message)
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
    (format t "Eval:  ~D ok, ~D errors~%" eval-ok eval-err)
    (format t "~%Tests Passed: ~D~%" total-passed)
    (format t "Tests Failed: ~D~%" total-failed)

    ;; Show error details for failed tests
    (let ((error-results (remove-if-not
                          (lambda (r) (eq (test-result-eval-status r) :error))
                          results)))
      (when error-results
        (format t "~%=== Error Details ===~%")
        (dolist (r (subseq error-results 0 (min 10 (length error-results))))
          (format t "~A: ~A~%" (test-result-file r)
                  (let ((msg (test-result-message r)))
                    (if (> (length msg) 100)
                        (concatenate 'string (subseq msg 0 100) "...")
                        msg))))
        (when (> (length error-results) 10)
          (format t "... and ~D more~%" (- (length error-results) 10)))))

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

  ;; Initialize eval system
  (init-eval-system)

  (let* ((results (run-all-tests))
         (all-passed (print-summary results)))

    (format t "~%")
    (if all-passed
        (format t "SUCCESS: All tests passed!~%")
        (format t "FAILED: Some tests failed or errored.~%"))

    ;; Exit with appropriate code
    (sb-ext:exit :code (if all-passed 0 1))))

;; Run if called as script
(eval-when (:execute)
  (main))
