(deftest test-vector-of
  (is (vector? (vector-of :int)))
  (is (= [] (vector-of :int)))
  (is (= [1 2 3] (vector-of :int 1 2 3))))
