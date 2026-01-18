#!/bin/sbcl --script

(load "package.lisp")
(load "cl-clojure-syntax.lisp")

(in-package :cl-clojure-syntax)

(let ((*readtable* (ensure-clojure-readtable)))
  (let ((form1 (with-input-from-string (s "{:via [{:keys [data]}]}")
                 (read s)))
        (form2 (with-input-from-string (s "{[{:keys [data]}] :via data-top-level :data}")
                 (read s))))
    (format t "Form 1: ~A~%" form1)
    (let ((keys nil))
      (maphash (lambda (k v) (declare (ignore v)) (push k keys)) form1)
      (format t "Form 1 keys: ~A~%" keys)
      (format t "Form 1 type of first key: ~A~%" (type-of (car keys))))
    (format t "~%Form 2: ~A~%" form2)
    (let ((keys nil))
      (maphash (lambda (k v) (declare (ignore v)) (push k keys)) form2)
      (format t "Form 2 keys: ~A~%" keys))
    (maphash (lambda (k v)
               (format t "  Key: ~A (type: ~A) Value: ~A (type: ~A)~%" k (type-of k) v (type-of v)))
             form2)))
