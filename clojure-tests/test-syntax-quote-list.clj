(ns test-syntax-quote-list)

(defmacro test-sq []
  `'foo)

(test-sq)
