;;;; -*- Mode: Lisp -*-

(load "package.lisp")
(load "cl-clojure-syntax.lisp")
(load "cl-clojure-eval.lisp")

(in-package #:cl-clojure-eval)

(defun test-keyword-lookup ()
  (let ((*readtable* cl-clojure-syntax:*clojure-readtable*)
        (value-map (make-hash-table :test #'equal)))
    ;; Add :|a| and :|b| to the map (as reader would create them)
    (setf (gethash :|a| value-map) 1)
    (setf (gethash :|b| value-map) 2)

    (format t "value-map keys:~%")
    (loop for k being the hash-keys of value-map
          do (format t "  ~a -> ~a~%" k (symbol-name k)))

    (format t "~%Looking for :A (uppercase):~%")
    (let ((found (loop for k being the hash-keys of value-map
                       when (string-equal (symbol-name k) "A")
                       return k)))
      (format t "  Found: ~a~%" found)
      (when found (format t "  Value: ~a~%" (gethash found value-map))))

    (format t "~%Looking for keyword :a in map: ~a~%" (gethash :a value-map))
    (format t "Looking for keyword :|a| in map: ~a~%" (gethash :|a| value-map))

    ;; Test what reader creates
    (format t "~%Reader test:~%")
    (format t "  (read-from-string \":a\") = ~a~%" (read-from-string ":a"))
    (format t "  symbol-name of that = ~a~%" (symbol-name (read-from-string ":a")))
    (format t "  :a eq :|a| = ~a~%" (eq (read-from-string ":a") :|a|))
    (format t "  equal of :a and :|a| = ~a~%" (equal (read-from-string ":a") :|a|))

    ;; Test with actual binding
    (format t "~%~%Testing actual destructuring:~%")
    (let* ((binding-map (read-from-string "{:keys (:a :b)}"))
           (env (make-instance 'env))
           (extended (extend-map-binding env binding-map value-map)))
      (format t "Extended env bindings: ~a~%" (env-bindings extended))
      (format t "Lookup |a|: ~a~%" (env-get-lexical extended '|a|))
      (format t "Lookup |b|: ~a~%" (env-get-lexical extended '|b|)))))

(test-keyword-lookup)
