;;;; -*- Mode: Lisp -*-

(load "package.lisp")
(load "cl-clojure-syntax.lisp")
(load "cl-clojure-eval.lisp")

(in-package :cl-clojure-eval)

;; Ensure readtable is initialized
(cl-clojure-syntax:ensure-clojure-readtable)

(let ((*readtable* cl-clojure-syntax:*clojure-readtable*))
  ;; Check what binding-form looks like
  (let* ((form-str "{:keys (:a :b)}")
         (form (cl-clojure-syntax:read-clojure-string form-str)))
    (format t "form type: ~a~%" (type-of form))
    (format t "form: ~a~%" form)
    (format t "form is hash-table: ~a~%" (hash-table-p form))
    (when (hash-table-p form)
      (format t "form count: ~a~%" (hash-table-count form))
      (format t "form entries:~%")
      (maphash (lambda (k v)
                 (format t "  k=~a (type=~a, symbol-name=~s) -> v=~a~%"
                         k (type-of k) (symbol-name k) v))
               form)))

  ;; Check what value-map looks like
  (let* ((m-str "{:a 1 :b 2}")
         (m (cl-clojure-syntax:read-clojure-string m-str)))
    (format t "~%m type: ~a~%" (type-of m))
    (format t "m: ~a~%" m)
    (when (hash-table-p m)
      (format t "m count: ~a~%" (hash-table-count m))
      (format t "m entries:~%")
      (maphash (lambda (k v)
                 (format t "  k=~a (type=~a, symbol-name=~s) -> v=~a~%"
                         k (type-of k) (symbol-name k) v))
               m))))
