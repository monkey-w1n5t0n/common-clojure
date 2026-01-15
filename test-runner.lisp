#!/bin/sbcl --script

;;;; Clojure Test Runner for Common Lisp
;;;; This attempts to load and run Clojure test files, reporting what works and what doesn't

(require 'asdf)
(load "cl-clojure-syntax.asd")
(asdf:load-system :cl-clojure-syntax)

(defpackage #:clojure-test-runner
  (:use #:cl #:cl-clojure-syntax))

(in-package #:clojure-test-runner)

;;; Test Results Tracking

(defstruct test-result
  file
  status        ; :parse-error :load-error :run-pending :run-pass :run-fail
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

;;; Try to read a Clojure file and report results
(defun try-read-clojure-file (path)
  "Try to read a Clojure file. Returns :success if readable, :error if not."
  (handler-case
      (progn
        (with-open-file (s path :direction :input)
          ;; Use Clojure readtable only for reading the file content
          (let ((*readtable* (ensure-clojure-readtable)))
            (loop for form = (read s nil :eof)
                  until (eq form :eof)
                  count t)))
        :success)
    (error (c)
      (declare (ignore c))
      :error)))

;;; Categorize test files based on what features they need
(defun categorize-test-file (filename)
  "Categorize a test file based on its name and content."
  (let ((name (pathname-name filename)))
    (cond
      ;; Reader-level features (what we currently implement)
      ((member name '("vectors" "data_structures" "keywords" "symbols"
                     "metadata" "numbers" "strings" "reader")
            :test #'string=)
       :reader)
      ;; Core language features
      ((member name '("control" "for" "fn" "evaluation" "compilation"
                     "special" "macros" "def")
            :test #'string=)
       :core)
      ;; Collections
      ((member name '("sequences" "clojure_set" "clojure_walk" "transducers"
                     "reducers" "transients")
            :test #'string=)
       :collections)
      ;; Concurrency
      ((member name '("refs" "atoms" "agents" "volatiles" "delays"
                     "futures" "promises")
            :test #'string=)
       :concurrency)
      ;; Java interop
      ((member name '("java_interop" "data_structures_interop" "genclass"
                     "proxy" "array_symbols")
            :test #'string=)
       :java)
      ;; Namespaces
      ((member name '("ns_libs" "api" "vars")
            :test #'string=)
       :namespaces)
      ;; Other
      (t :other))))

;;; Scan all test files and report status
(defun scan-all-tests ()
  "Scan all test files and categorize them by readiness."
  (let ((files (directory (merge-pathnames "*.clj" *test-dir*))))
    (format t "~%=== Clojure Test Suite Scan ===~%")
    (format t "Found ~D test files~%~%" (length files))

    ;; Group by category
    (let ((by-category (make-hash-table)))
      ;; Categorize all files
      (dolist (file files)
        (let ((cat (categorize-test-file file)))
          (push file (gethash cat by-category))))

      ;; Print summary by category
      (dolist (cat '(:reader :core :collections :concurrency :java :namespaces :other))
        (let ((cat-files (gethash cat by-category)))
          (when cat-files
            (format t "~%~A (~D files):~%" (string-capitalize cat) (length cat-files))
            (dolist (file (sort cat-files #'string< :key #'pathname-name))
              (format t "  - ~A~%" (pathname-name file)))))))

    ;; Parse status check
    (format t "~%~%=== Parse Status ===~%")
    (let ((readable 0)
          (error-count 0))
      (dolist (file files)
        (let ((status (try-read-clojure-file file)))
          (if (eq status :success)
              (incf readable)
              (progn
                (incf error-count)
                (format t "ERROR: ~A~%" (pathname-name file))))))
      (format t "~%Readable: ~D~%Errors:   ~D~%" readable error-count)))

  (values))

;;; Attempt to "run" a specific test file
(defun run-test-file (filename)
  "Attempt to load/run a single test file."
  (let ((path (merge-pathnames filename *test-dir*)))
    (format t "~%=== Running ~A ===~%" filename)
    (handler-case
        (progn
          (format t "Status: IMPLEMENTATION NEEDED~%")
          (format t "Clojure evaluation not yet implemented.~%")
          :not-implemented)
      (error (c)
        (format t "Error: ~A~%" c)
        :error))))

;;; Main entry point
(defun main (&optional (test-name nil))
  (format t "Clojure on SBCL - Test Runner~%")
  (format t "================================~%")
  (scan-all-tests)

  (when test-name
    (run-test-file test-name))

  (format t "~%~%Next: Implement features to make tests pass!~%"))

;; Run if called as script
(eval-when (:execute)
  (main))
