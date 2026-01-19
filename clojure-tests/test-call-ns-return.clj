(ns test-call-ns-return)

(defmacro call-ns-sym 
  [] `(do (ns a#) 'a#))

(call-ns-sym)
