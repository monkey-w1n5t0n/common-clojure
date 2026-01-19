;;;; -*- Mode: Lisp -*-

(load "package.lisp")
(load "cl-clojure-syntax.lisp")
(load "cl-clojure-eval.lisp")

(in-package :cl-clojure-eval)

(use-package :cl-clojure-syntax)

;; Read the destructuring form
(let* ((*readtable* *clojure-readtable*)
       (form (read-from-string "{:keys (:a :b)}"))
       (m (read-from-string "{:a 1 :b 2}"))
       (env (make-instance 'env))
       (result (extend-binding env form m)))

  (format t "binding-form type: ~a~%" (type-of form))
  (format t "binding-form: ~a~%" form)
  (format t "value-map type: ~a~%" (type-of m))
  (format t "value-map: ~a~%" m)

  (when (hash-table-p form)
    (format t "binding-form is hash-table with ~a entries~%" (hash-table-count form))
    (format t "binding-form entries:~%")
    (maphash (lambda (k v)
               (format t "  ~a -> ~a~%" k v))
             form))

  (format t "~nresult bindings: ~a~%" (env-bindings result))

  ;; Check lookup
  (format t "~%Lookup results:~%")
  (let ((a-val (env-get-lexical result '|a|))
        (b-val (env-get-lexical result '|b|)))
    (format t "  |a|: ~a~%" a-val)
    (format t "  |b|: ~a~%" b-val)))
