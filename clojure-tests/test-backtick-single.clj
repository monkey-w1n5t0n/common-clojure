(ns test-backtick-single)

(defmacro return-single []
  `'foo)

(return-single)
