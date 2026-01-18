(deftest test-let-intvecs
  (let [nums (range 1 100)
        num-seqs {:standard nums
                  :longer (concat nums [100])}
        create-vals (fn[base-val]
                      (zipmap (keys num-seqs)
                              (map #(into base-val %1) (vals num-seqs))))
        int-vecs (create-vals (vector-of :int))]
    (is (= 1 1))))
