(ns test-quote-macro)

(defmacro return-quote []
  (quote foo))

(return-quote)
