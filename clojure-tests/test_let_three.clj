(deftest test-let-three
  (let [nums (range 1 100)
        num-seqs {:standard nums}
        create-vals (fn[base-val] base-val)]
    (is (= 1 1))))
