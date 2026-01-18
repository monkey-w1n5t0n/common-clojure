(deftest test-zipmap-simple
  (let [ks '(:a :b :c)
        vs '(1 2 3)]
    (is (= 1 (:a (zipmap ks vs))))))
