(ns test-both
  (:require [clojure.math :as m]))

(set! *warn-on-reflection* true)

(defn neg-zero? [^double d]
  (and (zero? d) (< (Double/compare d 0.0) 0)))

(defn pos-zero? [^double d]
  (and (zero? d) (not (< (Double/compare d 0.0) 0))))

(neg-zero? -0.0)
(pos-zero? 0.0)
