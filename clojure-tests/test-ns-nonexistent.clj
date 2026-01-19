(ns test-ns-nonexistent
  (:use clojure.test
        non-existent.ns))

(deftest test-simple
  (is (= 1 1)))
