(ns test-call-ns)

(defmacro call-ns 
  "Call ns with a unique namespace name. Return the result of calling ns"
  []  `(ns a#))
