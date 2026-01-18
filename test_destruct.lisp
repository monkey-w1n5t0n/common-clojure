#!/bin/sbcl --script

;;;; Test nested destructuring

(load "package.lisp")
(load "cl-clojure-syntax.lisp")
(load "cl-clojure-eval.lisp")
(load "cl-clojure-transducers.lisp")

(in-package :cl-clojure-eval)

(init-eval-system)

;; Test the destructuring issue - need to use Clojure readtable
(format t "Testing nested destructuring...~%")
(let* ((code "(let [{[{:keys [data]}] :via data-top-level :data} (Throwable->map (ex-info \"ex-info\" {:some \"data\"}))] data-top-level)")
       (form (with-input-from-string (s code)
               (let ((*readtable* (ensure-clojure-readtable)))
                 (read-clojure s nil :eof))))
       (ex-form-clojure (with-input-from-string (s "(Throwable->map (ex-info \"ex-info\" {:some \"data\"}))")
                          (let ((*readtable* (ensure-clojure-readtable)))
                            (read-clojure s nil :eof)))))
  (format t "Form read: ~A~%" form)
  ;; First, test Throwable->map directly
  (format t "~%Testing Throwable->map directly...~%")
  (let ((result (clojure-eval ex-form-clojure *current-env*)))
    (format t "Throwable->map result: ~A~%" result)
    (format t "Result is hash-table: ~A~%" (hash-table-p result))
    (when (hash-table-p result)
      (format t "Result keys: ~A~%" (loop for k being the hash-keys of result collect k))
      (format t "Result :via: ~A~%" (gethash :via result))
      (format t "Result :data: ~A~%" (gethash :data result))
      (let ((via-val (gethash :via result)))
        (when (and via-val (vectorp via-val) (> (length via-val) 0))
          (let ((first-via (aref via-val 0)))
            (format t "First via element: ~A~%" first-via)
            (when (hash-table-p first-via)
              (format t "First via keys: ~A~%" (loop for k being the hash-keys of first-via collect k))
              (format t "First via :data: ~A~%" (gethash :data first-via))))))))
  (format t "~%Now testing full let form...~%")
  (let ((result (clojure-eval form *current-env*)))
    (format t "Result: ~A~%" result)))
