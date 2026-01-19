(ns test-call-ns-simple-2
  (:use clojure.test))

(defmacro call-ns 
  [] `(ns a#))

(deftest test-call-ns-simple
  (is (= nil (call-ns))))
