(ns test-require-gensym
  (:use clojure.test))

(defmacro call-ns-sym 
  [] `(do (ns a#) 'a#))

(deftest test-require-gensym
  (is (= nil (let [a (call-ns-sym)] (require a)))))
