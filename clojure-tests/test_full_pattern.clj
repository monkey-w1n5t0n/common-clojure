(deftest test-full-pattern
  (let [nums (range 1 10)
        num-seqs {:standard nums
                  :longer (concat nums [100])}
        create-vals (fn[base-val]
                      (zipmap (keys num-seqs)
                              (map #(into base-val %1) (vals num-seqs))))
        int-vecs (create-vals (vector-of :int))]
    (is (= 9 (count (:standard int-vecs))))
    (is (= 10 (count (:longer int-vecs))))))
