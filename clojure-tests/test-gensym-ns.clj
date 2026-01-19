(ns test-gensym-ns)

(defmacro make-ns-call []
  `(ns foo#))

(make-ns-call)
