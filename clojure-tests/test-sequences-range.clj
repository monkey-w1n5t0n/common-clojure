(ns test-sequences-range
  (:require [clojure.test :refer :all]))

(deftest test-range
  (let [arange (range 1 100)
        avec (into [] arange)]
    (is (= 4950 (reduce + avec)))))
