(ns test-call-ns-sym
  (:use clojure.test))

(defmacro call-ns-sym 
  "Call ns with a unique namespace name. Return the namespace symbol."
  [] `(do (ns a#) 'a#))

(deftest test-ns-macro
  (is (some? (call-ns-sym))))
