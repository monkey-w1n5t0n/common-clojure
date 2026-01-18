(deftest test-anon-fn
  (let [nums '(1 2 3)]
    (is (= '(2 3 4) (map #(+ %1 1) nums)))))
