(ns test-debug-require)

(defmacro call-ns-sym 
  [] `(do (ns a#) 'a#))

(call-ns-sym)
