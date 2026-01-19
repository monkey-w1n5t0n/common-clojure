(ns test-sequences-simple
  (:use clojure.test))

(deftest test-simple-reduce
  (is (= 6 (reduce + [1 2 3]))))

(deftest test-vector-of-int
  (is (vector? (vector-of :int 1 2 3))))
