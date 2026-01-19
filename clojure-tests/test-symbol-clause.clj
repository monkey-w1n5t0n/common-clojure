(ns test-symbol-clause)

(defmacro return-symbol-2 []
  (list 'quote 'foo))

(return-symbol-2)
