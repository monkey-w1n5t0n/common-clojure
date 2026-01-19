(load "cl-clojure-syntax.lisp")
(in-package #:cl-clojure-syntax)
(enable-clojure-syntax)
;; Test keyword reading
(let ((kw (read-from-string ":cause")))
  (format t "Keyword: ~a~%" kw)
  (format t "Keyword name: ~a~%" (symbol-name kw))
  (format t "Keyword package: ~a~%" (symbol-package kw)))
;; Test symbol reading
(let ((sym (read-from-string "cause")))
  (format t "Symbol: ~a~%" sym)
  (format t "Symbol name: ~a~%" (symbol-name sym))
  (format t "Symbol package: ~a~%" (symbol-package sym)))
;; Test keyword from symbol
(let ((sym (read-from-string "cause")))
  (let ((kw (intern (symbol-name sym) "KEYWORD")))
    (format t "Keyword from symbol: ~a~%" kw)
    (format t "Keyword name: ~a~%" (symbol-name kw))))
