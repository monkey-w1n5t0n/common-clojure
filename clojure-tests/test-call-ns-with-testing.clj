(ns test-call-ns-with-testing
  (:use clojure.test))

(defmacro call-ns 
  []  `(ns a#))

(deftest test-simple
  (call-ns))
