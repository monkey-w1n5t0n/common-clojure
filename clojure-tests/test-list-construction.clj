(ns test-list-construction)

(defmacro return-list-2 []
  (list 'quote 'foo))

(return-list-2)
