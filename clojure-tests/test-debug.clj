(ns test-debug)

(defmacro return-symbol []
  `'foo)

(return-symbol)
