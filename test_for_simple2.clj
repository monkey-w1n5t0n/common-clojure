;; Test for with :when
(for [x (range 4) :when (odd? x)] x)
