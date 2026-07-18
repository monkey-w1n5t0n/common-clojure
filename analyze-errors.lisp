;;;; Error analysis script for specific failing test files
;;;; Usage: sbcl --noinform --disable-debugger --non-interactive --load analyze-errors.lisp
;;;; Output goes to /tmp/clojure-error-analysis.txt

;; Load system with standard readtable first
(load "sbcl-init.lisp")
(load "package.lisp")
(load "cl-clojure-syntax.lisp")

;; Save standard readtable
(defparameter *standard-rt* (copy-readtable nil))

;; Load eval with standard readtable to avoid Clojure reader issues
(let ((*readtable* *standard-rt*))
  (load "cl-clojure-eval.lisp")
  (load "cl-clojure-transducers.lisp"))

;; Now switch to eval package
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
  "Diagnose a single test file. Returns a plist with results."
  (let ((path (make-pathname :name name :type "clj" :defaults *test-dir*))
        (result (list :name name :status :unknown :error-type nil
                      :error-msg "" :form-num nil :first-form nil)))
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
                              (read-sequence str s)
                              str)))
                 (preprocessed (cl-clojure-syntax:preprocess-clojure-dots content))
                 (comment-marker (cl-clojure-syntax:get-comment-marker))
                 (form-count 0)
                 (last-good-form nil))
            (with-input-from-string (stream preprocessed)
              (let ((*readtable* (cl-clojure-syntax:ensure-clojure-readtable)))
                (loop for form = (cl-clojure-syntax:read-clojure stream nil :eof)
                      until (eq form :eof)
                      do (unless (eq form comment-marker)
                           (incf form-count)
                           (handler-case
                               (progn
                                 (setf last-good-form form)
                                 (clojure-eval form *current-env*))
                             (error (c)
                               (return-from diagnose-one-file
                                 (list :name name
                                       :status :failed
                                       :error-type :eval-error
                                       :error-msg (format nil "~A" c)
                                       :form-num form-count
                                       :first-form (format nil "~A" form))))))))))
          ;; If we got here, all forms evaluated successfully
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
      (sb-int:simple-program-error (c)
        (list :name name :status :failed :error-type :program-error
              :error-msg (format nil "~A" c) :form-num nil :first-form nil))
      (cl:error (c)
        (list :name name :status :failed :error-type :error
              :error-msg (format nil "~A" c) :form-num nil :first-form nil)))))

(defun truncate-str (str max-len)
  (if (> (length str) max-len)
      (concatenate 'string (subseq str 0 (- max-len 3)) "...")
      str))

(defun first-line (str)
  (let ((pos (position #\Newline str)))
    (if pos (subseq str 0 pos) str)))

(defun classify-root-cause (error-type error-msg)
  "Analyze the error to determine root cause and difficulty."
  (let ((msg (string-downcase error-msg)))
    (cond
      ;; Reader/parse errors
      ((member error-type (list :reader-error :eof-error :stream-error))
       (cond
         ((search "dispatch macro" msg)
          (list :root-cause "Missing dispatch macro character in reader"
                :fix "Add reader macro for Clojure syntax"
                :difficulty :HARD))
         ((search "unmatched" msg)
          (list :root-cause "Unmatched delimiter in test file"
                :fix "Fix reader delimiter handling"
                :difficulty :MEDIUM))
         (t
          (list :root-cause "Reader cannot parse Clojure syntax"
                :fix "Extend Clojure reader support"
                :difficulty :MEDIUM))))

      ;; Unknown special form
      ((search "unknown special form" msg)
       (let* ((start (search "form: " msg))
              (form-name (if start (subseq msg (+ start 6)) "unknown")))
         (list :root-cause (format nil "Missing special form: ~A" form-name)
               :fix (format nil "Implement ~A special form" form-name)
               :difficulty :MEDIUM)))

      ;; Unknown function
      ((search "unknown function" msg)
       (let* ((start (search "function: " msg))
              (fn-name (if start (subseq msg (+ start 10) (min (+ start 50) (length msg))) "unknown")))
         ;; Trim fn-name at first paren/space
         (let ((trim-pos (or (position #\) fn-name) (position #\Space fn-name) (length fn-name))))
           (setf fn-name (subseq fn-name 0 trim-pos)))
         (list :root-cause (format nil "Missing core function: ~A" fn-name)
               :fix (format nil "Add ~A to core functions" fn-name)
               :difficulty :EASY)))

      ;; Cannot apply non-function
      ((search "cannot apply non-function" msg)
       (list :root-cause "Trying to call a non-function value"
             :fix "Register the function or fix the binding"
             :difficulty :MEDIUM))

      ;; Variable unbound / void variable
      ((or (search "unbound" msg) (search "void variable" msg))
       (list :root-cause "Referenced variable not found in environment"
             :fix "Add the variable/function to the environment"
             :difficulty :EASY))

      ;; Stack overflow
      ((or (search "stack overflow" msg) (search "control stack" msg))
       (list :root-cause "Stack overflow - infinite recursion"
             :fix "Fix recursive evaluation or add tail-call support"
             :difficulty :HARD))

      ;; Type error
      ((eq error-type :type-error)
       (list :root-cause "Type mismatch during evaluation"
             :fix "Add type checking/coercion"
             :difficulty :MEDIUM))

      ;; Java interop
      ((or (search "java" msg) (search "interop" msg) (search "jvm" msg))
       (list :root-cause "Java interop required"
             :fix "Implement Java interop bridge"
             :difficulty :HARD))

      ;; Program error
      ((eq error-type :program-error)
       (list :root-cause "Program error during evaluation"
             :fix "Fix argument handling or syntax"
             :difficulty :MEDIUM))

      ;; Default
      (t
       (list :root-cause "Other evaluation error"
             :fix "Investigate specific error"
             :difficulty :MEDIUM)))))

;; Run analysis ONCE
(let ((results nil)
      (output-path "/tmp/clojure-error-analysis.txt"))
  (format t "~%=== Analyzing ~D Test Files ===~%~%" (length *target-tests*))

  (dolist (name *target-tests*)
    (format t "  Diagnosing ~A...~%" name)
    (finish-output)
    (let ((result (diagnose-one-file name)))
      (push result results)))

  (setf results (nreverse results))

  ;; Write detailed output to file
  (with-open-file (out output-path :direction :output :if-exists :supersede)
    ;; Detailed results
    (format out "=== DETAILED ERROR ANALYSIS ===~%~%")

    (dolist (result results)
      (let ((name (getf result :name))
            (status (getf result :status))
            (error-type (getf result :error-type))
            (error-msg (getf result :error-msg))
            (form-num (getf result :form-num))
            (first-form (getf result :first-form)))
        (format out "=== ~A ===~%" name)
        (format out "  Status: ~A~%" status)
        (when (eq status :failed)
          (format out "  Error Type: ~A~%" error-type)
          (format out "  First Error: ~A~%" (truncate-str (first-line error-msg) 200))
          (when form-num
            (format out "  Failed at form #: ~A~%" form-num))
          (when first-form
            (format out "  Failing form (truncated): ~A~%" (truncate-str first-form 200)))
          ;; Classify
          (let ((classification (classify-root-cause error-type error-msg)))
            (format out "  Root Cause: ~A~%" (getf classification :root-cause))
            (format out "  Fix Needed: ~A~%" (getf classification :fix))
            (format out "  Difficulty: ~A~%" (getf classification :difficulty))))
        (format out "~%")))

    ;; Summary table sorted by difficulty
    (let ((sorted-results
            (sort results
                  (lambda (a b)
                    (let ((diff-a (getf (classify-root-cause
                                         (getf a :error-type)
                                         (getf a :error-msg))
                                        :difficulty))
                          (diff-b (getf (classify-root-cause
                                         (getf b :error-type)
                                         (getf b :error-msg))
                                        :difficulty)))
                      (< (position diff-a (list :EASY :MEDIUM :HARD))
                         (position diff-b (list :EASY :MEDIUM :HARD))))))))
      (format out "~%~%=== SUMMARY TABLE (sorted by difficulty) ===~%~%")
      (format out "~20A | ~10A | ~50A | ~40A | ~10A~%"
              "Test File" "Status" "First Error" "Root Cause" "Difficulty")
      (format out "~A~%" (make-string 135 :initial-element #\-))
      (dolist (result sorted-results)
        (let* ((name (getf result :name))
               (status (getf result :status))
               (error-type (getf result :error-type))
               (error-msg (getf result :error-msg))
               (classification (classify-root-cause error-type error-msg))
               (root-cause (getf classification :root-cause))
               (difficulty (getf classification :difficulty))
               (short-error (if (eq status :passed)
                                "PASSED"
                                (truncate-str (first-line error-msg) 48)))
               (short-cause (truncate-str root-cause 38)))
          (format out "~20A | ~10A | ~50A | ~40A | ~10A~%"
                  name status short-error short-cause difficulty)))))

  (format t "~%Analysis written to ~A~%" output-path))

(sb-ext:exit :code 0)
