(ns test-byte-array
  (:require [clojure.test :refer :all]))

(deftest test-byte-array
  (let [byte-array (into-array Byte/TYPE (map byte (range 1 10)))]
    (is (vector? byte-array)))))
