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
    (format t "form count: ~a~%" (hash-table-count form))
    (maphash (lambda (k v)
               (format t "  k=~a (type=~a, keywordp=~a, symbolp=~a, symbol-name=~s) -> v=~a (type=~a)~%"
                       k (type-of k) (keywordp k) (symbolp k) (symbol-name k)
                       v (type-of v))
               (when (consp v)
                 (format t "    v is a list, elements:~%")
                 (dolist (elem v)
                   (format t "      elem=~a (type=~a, keywordp=~a, symbolp=~a)~%"
                           elem (type-of elem) (keywordp elem) (symbolp elem)))))
             form))
  )
