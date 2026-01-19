(ns test-char-array
  (:require [clojure.test :refer :all]))

(deftest test-char-array
  (let [char-array (into-array Character/TYPE (map char (range 1 10)))]
    (is (vector? char-array)))))
