(ns test-special-exact)

(let [m {:a/b 1 :c/d 2}]
  (let [{:keys [:a/b :c/d]} m]
    (is (= [1 2] [b d]))))
