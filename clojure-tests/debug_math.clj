(ns debug-math)

(set! *warn-on-reflection* true)

(defn neg-zero? [^double d]
  (and (zero? d) (< (Double/compare d 0.0) 0)))

(neg-zero? -0.0)
