(ns test-int-vec
  (:require [clojure.test :refer :all]))

(deftest test-int-vec
  (let [int-vec (into (vector-of :int) (range 1 10))]
    (is (vector? int-vec)))))
