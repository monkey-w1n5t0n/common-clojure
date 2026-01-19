(ns test-into-array
  (:require [clojure.test :refer :all]))

(deftest test-into-array
  (let [arange (range 1 10)
        obj-array (into-array arange)]
    (is (= 9 (reduce + obj-array)))))
