;;;; Diagnostic code - fed to SBCL via stdin after loading the system
;;;; This avoids the Clojure readtable interfering with loading

(in-package :cl-clojure-eval)

(defparameter *diag-test-dir*
  (make-pathname :directory (append (pathname-directory *load-truename*)
                                    (list "clojure-tests"))))

(defparameter *failing-tests*
  (list "agents" "array_symbols" "clearing" "clojure_walk" "clojure_xml"
        "compilation" "data_structures" "errors" "evaluation" "for"
        "genclass" "java_interop" "main" "metadata" "method_thunks"
        "multimethods" "other_functions" "param_tags" "parse" "predicates"
        "printer" "protocols" "reducers" "reflect" "rt" "sequences"
        "serialization" "special" "streams" "string" "test"
        "test_let_bindings" "transducers" "vectors"))

(defun string-search-ci (substr str)
  (search substr str :test #'char-equal))

(defun diagnose-file (name)
  (let ((path (make-pathname :name name :type "clj" :defaults *diag-test-dir*)))
    (unless (probe-file path)
      (return-from diagnose-file (values name :file-not-found "" nil)))
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
                               (return-from diagnose-file
                                 (values name :eval-error
                                         (format nil "~A" c)
                                         form-count))))))))))
          (values name :passed "" nil))
      (cl:reader-error (c)
        (values name :reader-error (format nil "~A" c) nil))
      (cl:stream-error (c)
        (values name :stream-error (format nil "~A" c) nil))
      (cl:end-of-file (c)
        (values name :eof-error (format nil "~A" c) nil))
      (cl:type-error (c)
        (values name :type-error (format nil "~A" c) nil))
      (cl:error (c)
        (values name :error (format nil "~A" c) nil))))

(defun categorize-failure (error-type error-msg)
  (cond
    ((member error-type (list :reader-error :eof-error :parse-error :stream-error))
     :missing-reader-features)
    ((string-search-ci "unknown special form" error-msg)
     :missing-special-forms)
    ((or (search "Unknown function" error-msg)
         (search "Cannot apply non-function" error-msg))
     :missing-core-functions)
    ((or (string-search-ci "java" error-msg)
         (string-search-ci "interop" error-msg)
         (string-search-ci "reflect" error-msg)
         (string-search-ci "genclass" error-msg))
     :java-interop)
    ((or (string-search-ci "stack overflow" error-msg)
         (string-search-ci "control stack" error-msg))
     :stack-overflow)
    (t :other-issues)))

(defun truncate-string (str max-len)
  (if (> (length str) max-len)
      (concatenate 'string (subseq str 0 (- max-len 3)) "...")
      str))

(defun first-line (str)
  (let ((pos (position #\Newline str)))
    (if pos (subseq str 0 pos) str)))

;; Run diagnostics
(format t "~%=== Diagnosing ~D Failing Test Files ===~%~%" (length *failing-tests*))

(let ((results nil))
  (dolist (name *failing-tests*)
    (multiple-value-bind (fname error-type error-msg form-num)
        (diagnose-file name)
      (push (list fname error-type error-msg form-num) results)))

  (setf results (nreverse results))

  ;; Print detailed results
  (format t "~%=== DETAILED RESULTS ===~%")
  (dolist (result results)
    (destructuring-bind (fname error-type error-msg form-num) result
      (format t "~%~%--- ~A ---~%" fname)
      (format t "  Error Type: ~A~%" error-type)
      (format t "  Error: ~A~%" (truncate-string error-msg 300))
      (when form-num
        (format t "  At form #: ~A~%" form-num))))

  ;; Print summary table
  (format t "~%~%=== SUMMARY TABLE ===~%")
  (format t "~%~40A | ~25A | ~A~%" "Test File" "Category" "First Error")
  (format t "~A~%" (make-string 120 :initial-element #\-))
  (dolist (result results)
    (destructuring-bind (fname error-type error-msg form-num) result
      (declare (ignore form-num))
      (let* ((category (categorize-failure error-type error-msg))
             (category-str (string-downcase (symbol-name category)))
             (short-error
               (cond
                 ((eq error-type :passed) "PASSED (unexpected)")
                 ((eq error-type :file-not-found) "File not found")
                 (t (truncate-string (first-line error-msg) 70)))))
        (format t "~40A | ~25A | ~A~%" fname category-str short-error)))))

(format t "~%~%Done.~%")
(sb-ext:exit :code 0)
