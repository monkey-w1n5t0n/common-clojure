(ns clojure.test-clojure.math-exact7
  (:require
    [clojure.test :refer :all]
    [clojure.math :as m]))

(set! *warn-on-reflection* true)

(defn ulp=
  "Tests that y = x +/- m*ulp(x)"
  [x y ^double m]
  (let [mu (* (m/ulp x) m)]
    (<= (- x mu) y (+ x mu))))

(deftest test-ulp
  (is (ulp= (m/sin m/PI) (m/sin m/PI) 1)))
