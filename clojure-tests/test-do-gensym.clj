(ns test-do-gensym)

(defmacro make-do []
  `(do (ns foo#) 'foo#))

(make-do)
