(ns test-special-first)

(deftest multiple-keys-in-destructuring
  (let [foo (fn [& {:keys [x]}] x)
        bar (fn [& options] (apply foo :x :b options))]
    (is (= (bar) :b))
    (is (= (bar :x :a) :a))))
