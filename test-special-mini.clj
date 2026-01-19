(ns test-special-mini)

(deftest keywords-in-destructuring-mini
  (let [m {:a 1 :b 2}]
    (let [{:keys [:a :b]} m]
      [a b])))
