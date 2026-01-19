(ns test-syntax-quote-gensym)

(defmacro return-gensym-quote []
  `'a#)

(return-gensym-quote)
