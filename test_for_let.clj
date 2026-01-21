;; Test for with :let
(for [x (range 2) :let [y (+ 1 x)]] [x y])
