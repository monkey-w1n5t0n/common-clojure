(ns test-debug-is)

(let [m {:a/b 1}]
  (let [{:keys [:a/b]} m]
    (is (= 1 b))))
