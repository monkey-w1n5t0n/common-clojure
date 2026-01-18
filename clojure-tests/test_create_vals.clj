(deftest test-create-vals
  (let [num-seqs {:a '(1 2 3) :b '(4 5 6)}
        create-vals (fn[base-val]
                      (zipmap (keys num-seqs)
                              (map #(into base-val %1) (vals num-seqs))))]
    (is (= '(1 2 3) (:a (create-vals []))))))
