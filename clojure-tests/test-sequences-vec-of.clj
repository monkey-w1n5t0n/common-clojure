(ns test-sequences-vec-of
  (:require [clojure.test :refer :all]))

(deftest test-vector-of-int
  (is (vector? (vector-of :int 1 2 3))))
