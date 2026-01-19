(ns test-destruct)

(let [m {:a/b 1 :c/d 2}]
  (let [{:keys [:a/b :c/d]} m]
    [b d]))
