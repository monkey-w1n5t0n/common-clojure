(deftest test-concat-pattern
  (let [nums (range 1 10)
        num-seqs {:standard nums
                  :longer (concat nums [100])}
        create-vals (fn[base-val]
                      (zipmap (keys num-seqs)
                              (map #(into base-val %1) (vals num-seqs))))]
    (is (= '(1 2 3 4 5 6 7 8 9) (:standard (create-vals []))))
    (is (= '(1 2 3 4 5 6 7 8 9 100) (:longer (create-vals []))))))
