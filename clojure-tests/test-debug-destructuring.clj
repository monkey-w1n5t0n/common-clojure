(ns test-debug-destructuring)

(let [m {:a/b 1}]
  (let [{:keys [:a/b]} m]
    b))
