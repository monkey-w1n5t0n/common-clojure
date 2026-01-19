(ns test-int-array
  (:require [clojure.test :refer :all]))

(deftest test-int-array
  (let [arange (range 1 10)
        int-array (into-array Integer/TYPE arange)]
    (is (= 9 (reduce + int-array)))))
