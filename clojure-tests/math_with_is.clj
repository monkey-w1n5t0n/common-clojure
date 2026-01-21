(ns test-math-is
  (:require [clojure.math :as m]))

(set! *warn-on-reflection* true)

(defn neg-zero?
  [^double d]
  (and (zero? d) (< (Double/compare d 0.0) 0)))

(deftest test-sin
  (is (neg-zero? (m/sin -0.0))))
