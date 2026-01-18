(deftest test-let-few
  (let [nums (range 1 100)
        num-seqs {:standard nums}]
    (is (= 99 (count (:standard num-seqs))))))
