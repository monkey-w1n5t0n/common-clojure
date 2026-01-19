(ns test-gensym-return)

(defmacro return-gensym []
  `'a#)

(return-gensym)
