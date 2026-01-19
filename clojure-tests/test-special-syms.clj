
(ns test-special-syms
  (:use clojure.test))

(deftest namespaced-syms-in-destructuring
  (let [{:syms [a/b c/d e/f] :or {f 3}} {'a/b 1 'c/d 2}]
    (is (= [1 2 3] [b d f]))))
