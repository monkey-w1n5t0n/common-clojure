(ns test-interned-symbol)

(defmacro return-interned []
  (intern "cl-clojure-eval" "FOO"))

(return-interned)
