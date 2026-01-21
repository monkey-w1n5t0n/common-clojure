;; Test for with :let and :when
(for [x (range 3) y (range 3) :let [z (+ x y)] :when (odd? z)] [x y z])
