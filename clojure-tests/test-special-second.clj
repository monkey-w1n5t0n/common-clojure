
(ns test-special-second)

(deftest empty-list-with-:as-destructuring
  (let [{:as x} '()]
    (is (= {} x))))
