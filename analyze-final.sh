#!/bin/bash
# Run error analysis for Clojure test files
# Usage: bash analyze-final.sh

cd /home/w1n5t0n/src/common-clojure

sbcl --noinform --disable-debugger --non-interactive \
  --load sbcl-init.lisp \
  --load package.lisp \
  --load cl-clojure-syntax.lisp \
  --eval '
(let ((*readtable* (copy-readtable nil)))
  (load "cl-clojure-eval.lisp")
  (load "cl-clojure-transducers.lisp"))' \
  --eval '
(in-package :cl-clojure-eval)

(defparameter *test-dir*
  (make-pathname :directory (append (pathname-directory *load-truename*)
                                    (list "clojure-tests"))))

(defparameter *target-tests*
  (list "agents" "array_symbols" "data_structures" "errors" "for"
        "metadata" "multimethods" "parse" "printer" "protocols"
        "reducers" "test" "test_let_bindings" "transducers" "vectors"
        "other_functions" "sequences" "special" "compilation"
        "clojure_walk" "clearing" "evaluation"))

(defun diagnose-one-file (name)
  (let ((path (make-pathname :name name :type "clj" :defaults *test-dir*)))
    (unless (probe-file path)
      (return-from diagnose-one-file
        (list :name name :status :file-not-found :error-type :file-not-found
              :error-msg "File not found" :form-num nil :first-form nil)))
    (setf *current-env* nil)
    (handler-case
        (progn
          (init-eval-system)
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
                               (clojure-eval form *current-env*)
                             (error (c)
                               (return-from diagnose-one-file
                                 (list :name name :status :failed
                                       :error-type :eval-error
                                       :error-msg (format nil "~A" c)
                                       :form-num form-count
                                       :first-form (format nil "~A" form))))))))))
          (list :name name :status :passed :error-type nil
                :error-msg "" :form-num nil :first-form nil))
      (cl:reader-error (c)
        (list :name name :status :failed :error-type :reader-error
              :error-msg (format nil "~A" c) :form-num nil :first-form nil))
      (cl:stream-error (c)
        (list :name name :status :failed :error-type :stream-error
              :error-msg (format nil "~A" c) :form-num nil :first-form nil))
      (cl:end-of-file (c)
        (list :name name :status :failed :error-type :eof-error
              :error-msg (format nil "~A" c) :form-num nil :first-form nil))
      (cl:type-error (c)
        (list :name name :status :failed :error-type :type-error
              :error-msg (format nil "~A" c) :form-num nil :first-form nil))
      (cl:error (c)
        (list :name name :status :failed :error-type :error
              :error-msg (format nil "~A" c) :form-num nil :first-form nil)))))

(defun truncate-str (str max-len)
  (if (> (length str) max-len)
      (concatenate (quote string) (subseq str 0 (- max-len 3)) "...")
      str))

(defun first-line (str)
  (let ((pos (position (quote #\Newline) str)))
    (if pos (subseq str 0 pos) str)))

(defun extract-undefined-symbol (msg)
  (let ((prefix "Undefined symbol: "))
    (let ((pos (search prefix msg)))
      (when pos
        (let* ((start (+ pos (length prefix)))
               (rest (subseq msg start))
               (end (or (position (quote #\Newline) rest) (length rest))))
          (string-trim " " (subseq rest 0 end)))))))

(defun classify-error (error-type error-msg)
  (let ((msg error-msg))
    (cond
      ((member error-type (list :reader-error :eof-error :stream-error))
       (list :root-cause "Reader/parse error"
             :fix "Extend Clojure reader support"
             :difficulty :MEDIUM
             :specific "reader"))
      ((search "Undefined symbol: " msg)
       (let ((sym (extract-undefined-symbol msg)))
         (list :root-cause (format nil "Missing core function: ~A" sym)
               :fix (format nil "Register function ~A in setup-core-functions" sym)
               :difficulty :EASY
               :specific sym)))
      ((search "Unknown special form" msg)
       (let* ((start (search "form: " msg))
              (form-name (if start (subseq msg (+ start 6)) "unknown")))
         (list :root-cause (format nil "Missing special form: ~A" form-name)
               :fix (format nil "Implement ~A special form" form-name)
               :difficulty :MEDIUM
               :specific form-name)))
      ((or (search "Throwable" msg) (search "just testing" msg))
       (list :root-cause "Java exception class reference"
             :fix "Add stubs for Java exception classes"
             :difficulty :MEDIUM
             :specific "java-exceptions"))
      ((or (search "stack overflow" (string-downcase msg))
           (search "control stack" (string-downcase msg)))
       (list :root-cause "Stack overflow"
             :fix "Fix recursive evaluation"
             :difficulty :HARD
             :specific "recursion"))
      (t
       (list :root-cause (truncate-str (first-line msg) 60)
             :fix "Investigate specific error"
             :difficulty :MEDIUM
             :specific "unknown")))))

(let ((results nil)
      (output-path "/tmp/clojure-error-analysis.txt"))
  (format t "~%Analyzing ~D test files...~%~%" (length *target-tests*))
  (dolist (name *target-tests*)
    (format t "  ~A...~%" name)
    (finish-output)
    (push (diagnose-one-file name) results))
  (setf results (nreverse results))
  (with-open-file (out output-path :direction :output :if-exists :supersede)
    (format out "DETAILED ERROR ANALYSIS FOR 22 NON-JAVA-INTEROP TEST FILES~%~%")
    (dolist (result results)
      (let ((name (getf result :name))
            (status (getf result :status))
            (error-type (getf result :error-type))
            (error-msg (getf result :error-msg))
            (form-num (getf result :form-num))
            (first-form (getf result :first-form)))
        (format out "=== ~A ===~%" name)
        (format out "  Status:     ~A~%" status)
        (when (eq status :failed)
          (format out "  Error Type: ~A~%" error-type)
          (format out "  First Error (exact): ~A~%" (truncate-str (first-line error-msg) 250))
          (when form-num (format out "  At form #:  ~A~%" form-num))
          (when first-form
            (format out "  Failing form (truncated): ~A~%" (truncate-str first-form 200)))
          (let ((c (classify-error error-type error-msg)))
            (format out "  Root Cause: ~A~%" (getf c :root-cause))
            (format out "  Fix Needed: ~A~%" (getf c :fix))
            (format out "  Difficulty: ~A~%" (getf c :difficulty))))
        (format out "~%")))
    (let ((sorted
            (sort (copy-list results)
                  (lambda (a b)
                    (let* ((ca (classify-error (getf a :error-type) (getf a :error-msg)))
                           (cb (classify-error (getf b :error-type) (getf b :error-msg)))
                           (da (getf ca :difficulty))
                           (db (getf cb :difficulty))
                           (order (list :EASY :MEDIUM :HARD)))
                      (cond
                        ((< (position da order) (position db order)) t)
                        ((> (position da order) (position db order)) nil)
                        (t (string< (getf a :name) (getf b :name)))))))))
      (format out "~%SUMMARY TABLE (sorted by difficulty, easiest first)~%~%")
      (format out "~20A | ~50A | ~40A | ~10A~%" "test-file" "exact-first-error" "root-cause" "difficulty")
      (format out "~A~%" (make-string 130 :initial-element (quote #\-)))
      (dolist (result sorted)
        (let* ((name (getf result :name))
               (status (getf result :status))
               (error-type (getf result :error-type))
               (error-msg (getf result :error-msg))
               (c (classify-error error-type error-msg))
               (short-error (if (eq status :passed) "PASSED" (truncate-str (first-line error-msg) 48)))
               (short-cause (truncate-str (getf c :root-cause) 38)))
          (format out "~20A | ~50A | ~40A | ~10A~%" name short-error short-cause (getf c :difficulty))))))
  (format t "Analysis written to ~A~%" output-path))

(sb-ext:exit :code 0)'
