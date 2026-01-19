;;;; -*- Mode: Lisp -*-

(load "package.lisp")
(load "cl-clojure-syntax.lisp")

(in-package :cl-clojure-syntax)

;; Ensure readtable is initialized
(ensure-clojure-readtable)

(let ((*readtable* *clojure-readtable*))
  (let ((kw (read-from-string ":a")))
    (format t "keyword: ~a~%" kw)
    (format t "keyword symbol-name: ~a~%" (symbol-name kw))
    (format t "keyword type: ~a~%" (type-of kw))
    (format t "keywordp: ~a~%" (keywordp kw))
    (let ((sym (intern (symbol-name kw) :cl)))
      (format t "interned symbol: ~a~%" sym)
      (format t "interned symbol-name: ~a~%" (symbol-name sym))
      (format t "string comparison: ~a~%" (string= (symbol-name kw) (symbol-name sym))))
    ;; Check what symbol is created when we read 'a'
    (let ((sym-read (read-from-string "a")))
      (format t "symbol from reading 'a': ~a~%" sym-read)
      (format t "its symbol-name: ~a~%" (symbol-name sym-read))
      (format t "its package: ~a~%" (symbol-package sym-read))
      (format t "string eq kw/sym: ~a~%" (string= (symbol-name kw) (symbol-name sym-read)))))
