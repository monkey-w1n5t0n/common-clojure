(ns clojure.test-clojure.math-exact2
  (:require
    [clojure.test :refer :all]
    [clojure.math :as m]))

(set! *warn-on-reflection* true)

(defn neg-zero?
  [^double d]
  (and (zero? d) (< (Double/compare d 0.0) 0)))

(defn pos-zero?
  [^double d]
  (and (zero? d) (not (< (Double/compare d 0.0) 0))))

(defn ulp=
  "Tests that y = x +/- m*ulp(x)"
  [x y ^double m]
  (let [mu (* (m/ulp x) m)]
    (<= (- x mu) y (+ x mu))))
