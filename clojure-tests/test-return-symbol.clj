(ns test-return-symbol)

(defmacro return-foo []
  (list 'quote 'foo))
(return-foo)
