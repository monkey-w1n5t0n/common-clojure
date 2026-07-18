;;;; Final Error Analysis Script
;;;; Usage: sbcl --noinform --disable-debugger --non-interactive --load analyze-errors3.lisp

(load "sbcl-init.lisp")
(load "package.lisp")
(load "cl-clojure-syntax.lisp")

(defparameter *standard-rt* (copy-readtable nil))

(let ((*readtable* *standard-rt*))
  (load "cl-clojure-eval.lisp")
  (load "cl-clojure-transducers.lisp"))

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
  "Diagnose a single test file."
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
      (concatenate 'string (subseq str 0 (- max-len 3)) "...")
      str))

(defun first-line (str)
  (let ((pos (position #\Newline str)))
    (if pos (subseq str 0 pos) str)))

(defun extract-undefined-symbol (msg)
  "Extract the symbol name from 'Undefined symbol: SYMNAME' error message.
   The format after 'Undefined symbol: ' is the printed representation of the form.
   For simple symbols this is just the symbol name.
   We need to handle cases like =, not, mapcat etc."
  (let ((prefix "Undefined symbol: "))
    (let ((pos (search prefix msg)))
      (when pos
        (let* ((start (+ pos (length prefix)))
               (rest (subseq msg start))
               ;; The symbol name goes to end of line
               (end (or (position #\Newline rest) (length rest))))
          (string-trim " " (subseq rest 0 end)))))))

(defun classify-error (error-type error-msg)
  "Classify the error with accurate root cause, fix, and difficulty."
  (let ((msg error-msg))
    (cond
      ;; Reader errors
      ((member error-type (list :reader-error :eof-error :stream-error))
       (list :root-cause "Reader/parse error"
             :fix "Extend Clojure reader support"
             :difficulty :MEDIUM
             :specific "reader"))

      ;; Undefined symbol
      ((search "Undefined symbol: " msg)
       (let ((sym (extract-undefined-symbol msg)))
         (cond
           ;; These are core functions that just need to be registered
           ((member sym (list "=" "not" "mapcat" "reduce" "range" "meta"
                             "find-ns" "hash-map") :test #'string=)
            (list :root-cause (format nil "Missing core function: ~A" sym)
                  :fix (format nil "Register '~A' in setup-core-functions" sym)
                  :difficulty :EASY
                  :specific sym))
           ;; Java interop symbols (contain /)
           ((find #\/ sym)
            (list :root-cause (format nil "Java interop needed: ~A" sym)
                  :fix (format nil "Implement Java interop for ~A" sym)
                  :difficulty :HARD
                  :specific sym))
           ;; Other missing symbols
           (t
            (list :root-cause (format nil "Missing core function: ~A" sym)
                  :fix (format nil "Register '~A' in setup-core-functions" sym)
                  :difficulty :EASY
                  :specific sym)))))

      ;; Unknown special form
      ((search "Unknown special form" msg)
       (let* ((start (search "form: " msg))
              (form-name (if start (subseq msg (+ start 6)) "unknown")))
         (list :root-cause (format nil "Missing special form: ~A" form-name)
               :fix (format nil "Implement ~A special form" form-name)
               :difficulty :MEDIUM
               :specific form-name)))

      ;; Throwable / Java exception
      ((or (search "Throwable" msg) (search "just testing" msg))
       (list :root-cause "Java exception class reference (Throwable)"
             :fix "Add stubs for Java exception classes + throw/catch"
             :difficulty :MEDIUM
             :specific "java-exceptions"))

      ;; Stack overflow
      ((or (search "stack overflow" (string-downcase msg))
           (search "control stack" (string-downcase msg)))
       (list :root-cause "Stack overflow - infinite recursion"
             :fix "Fix recursive evaluation"
             :difficulty :HARD
             :specific "recursion"))

      ;; Default
      (t
       (list :root-cause (truncate-str (first-line msg) 60)
             :fix "Investigate specific error"
             :difficulty :MEDIUM
             :specific "unknown"))))

;; Run analysis
(let ((results nil)
      (output-path "/tmp/clojure-error-analysis.txt"))
  (format t "~%=== Analyzing ~D Test Files ===~%~%" (length *target-tests*))

  (dolist (name *target-tests*)
    (format t "  ~A...~%" name)
    (finish-output)
    (push (diagnose-one-file name) results))

  (setf results (nreverse results))

  ;; Write to file
  (with-open-file (out output-path :direction :output :if-exists :supersede)

    (format out "=== DETAILED ERROR ANALYSIS FOR 22 NON-JAVA-INTEROP TEST FILES ===~%~%")

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
          (format out "  First Error (exact): ~A~%" (truncate-str (first-line error-msg) 200))
          (when form-num (format out "  At form #:  ~A~%" form-num))
          (when first-form
            (format out "  Failing form (truncated): ~A~%" (truncate-str first-form 150)))
          (let ((c (classify-error error-type error-msg)))
            (format out "  Root Cause: ~A~%" (getf c :root-cause))
            (format out "  Fix Needed: ~A~%" (getf c :fix))
            (format out "  Difficulty: ~A~%" (getf c :difficulty))))
        (format out "~%")))

    ;; Summary table sorted by difficulty
    (let ((sorted
            (sort (copy-list results)
                  (lambda (a b)
                    (let* ((ca (classify-error (getf a :error-type) (getf a :error-msg)))
                           (cb (classify-error (getf b :error-type) (getf b :error-msg)))
                           (da (getf ca :difficulty))
                           (db (getf cb :difficulty))
                           (diff-order (list :EASY :MEDIUM :HARD)))
                      (cond
                        ((< (position da diff-order) (position db diff-order)) t)
                        ((> (position da diff-order) (position db diff-order)) nil)
                        (t (string< (getf a :name) (getf b :name)))))))))
      (format out "~%~%=== SUMMARY TABLE (sorted by difficulty, easiest first) ===~%~%")
      (format out "~20A | ~45A | ~45A | ~10A~%"
              "test-file" "exact-first-error" "root-cause" "difficulty")
      (format out "~A~%" (make-string 130 :initial-element #\-))
      (dolist (result sorted)
        (let* ((name (getf result :name))
               (status (getf result :status))
               (error-type (getf result :error-type))
               (error-msg (getf result :error-msg))
               (c (classify-error error-type error-msg))
               (short-error (if (eq status :passed)
                                "PASSED"
                                (truncate-str (first-line error-msg) 43)))
               (short-cause (truncate-str (getf c :root-cause) 43)))
          (format out "~20A | ~45A | ~45A | ~10A~%"
                  name short-error short-cause (getf c :difficulty))))))

  (format t "~%Analysis written to ~A~%" output-path))

(sb-ext:exit :code 0)
