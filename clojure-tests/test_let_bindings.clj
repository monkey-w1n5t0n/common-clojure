(deftest test-let-bindings
  (let [nums (range 1 100)
        num-seqs {:standard nums
                  :empty '()
                  :longer (concat nums [100])
                  :shorter (drop-last nums)}
        create-vals (fn[base-val]
                      (zipmap (keys num-seqs)
                              (map #(into base-val %1) (vals num-seqs))))
        int-vecs (create-vals (vector-of :int))]
    (is (= 99 (count (:standard int-vecs))))
    (is (= 0 (count (:empty int-vecs))))
    (is (= 100 (count (:longer int-vecs)))))
