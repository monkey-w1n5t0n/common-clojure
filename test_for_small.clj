;; Small test for for comprehension
(deftest test-for-small
  (is (= (for [x (range 3) y (range 3) :let [z (+ x y)] :when (odd? z)] [x y z])
         '([0 1 1] [1 0 1] [1 2 3] [2 1 3]))))
