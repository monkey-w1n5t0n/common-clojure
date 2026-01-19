(ns test-rest-keys)

(let [foo (fn [& {:keys [x]}] x)
      bar (fn [& options] (apply foo :x :b options))]
  (is (= (bar) :b)))
