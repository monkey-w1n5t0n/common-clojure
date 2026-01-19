(ns test-call-ns-require
  (:use clojure.test))

(defmacro call-ns-sym 
  [] `(do (ns a#) 'a#))

(deftest test-call-ns-require
  (let [a (call-ns-sym)]
    (is (symbol? a))))
