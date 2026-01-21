(deftest test-let-concat
  (let [nums (range 1 100)
        num-seqs {:standard nums
                  :longer (concat nums [100])}
        create-vals (fn[base-val] base-val)]
    (is (= 1 1))))
