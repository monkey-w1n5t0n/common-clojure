(ns test-let-require
  (:use clojure.test))

(defmacro call-ns-sym 
  [] `(do (ns a#) 'a#))

(deftest test-let-require
  (let [a 'user]
    (is (= nil nil))))
