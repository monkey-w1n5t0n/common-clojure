(deftest test-zipmap-debug
  (let [nums (range 1 10)
        m {:a 1 :b 2 :c 3}]
    (is (= '(1 2 3) (vals m)))
    (is (= '(:a :b :c) (keys m)))
    (is (= {:a 1 :b 2 :c 3} m))))
