(ns test-syntax-quote)

(defmacro return-syntax-quote []
  `foo)

(return-syntax-quote)
