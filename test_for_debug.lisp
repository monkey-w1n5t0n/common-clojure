(load "package.lisp")
(load "cl-clojure-syntax.lisp")
(load "cl-clojure-eval.lisp")

(use-package :cl-clojure-syntax)
(use-package :cl-clojure-eval)

;; Enable Clojure syntax BEFORE reading test strings
(enable-clojure-syntax)

;; Simple test without :let
(format t "~&=== Test 1: Simple for without :let ===~%")
(let ((code "(for [x (range 2)] x)"))
  (format t "Testing: ~a~%" code)
  (let ((result (clojure-eval (read-from-string code) *global-env*)))
    (format t "Result: ~a~%" result)))

;; Test with :let
(format t "~&=== Test 2: for with :let ===~%")
(let ((code "(for [x (range 2) :let [y (+ 1 x)]] [x y])"))
  (format t "Testing: ~a~%" code)
  (let ((result (clojure-eval (read-from-string code) *global-env*)))
    (format t "Result: ~a~%" result)))

;; Test with :when
(format t "~&=== Test 3: for with :when ===~%")
(let ((code "(for [x (range 4) :when (odd? x)] x)"))
  (format t "Testing: ~a~%" code)
  (let ((result (clojure-eval (read-from-string code) *global-env*)))
    (format t "Result: ~a~%" result)))

;; Original failing test
(format t "~&=== Test 4: Original failing test with :let and :when ===~%")
(let ((code "(for [x (range 3) y (range 3) :let [z (+ x y)] :when (odd? z)] [x y z])"))
  (format t "Testing: ~a~%" code)
  (let ((result (clojure-eval (read-from-string code) *global-env*)))
    (format t "Result: ~a~%" result)))
