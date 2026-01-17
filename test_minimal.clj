(defn neg-zero? [d] (and (zero? d) (< (Double/compare d 0.0) 0)))
(neg-zero? -0.0)
