
(ns test-special-namespaced)

(deftest namespaced-keywords-in-destructuring
  (let [m {:a/b 1 :c/d 2}]
    (let [{:keys [:a/b :c/d]} m]
      (is (= [1 2] [b d])))
    (let [{:keys [:a/b :c/d :e/f] :or {f 3}} m]
      (is (= [1 2 3] [b d f])))))
