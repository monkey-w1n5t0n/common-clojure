(ns test-repl-exact
  (:use clojure.test))

(defmacro call-ns 
  "Call ns with a unique namespace name. Return the result of calling ns"
  []  `(ns a#))

(defmacro call-ns-sym 
  "Call ns wih a unique namespace name. Return the namespace symbol."
  [] `(do (ns a#) 'a#))

(deftest test-dynamic-ns
  (testing "a call to ns returns nil"
   (is (= nil (call-ns))))
  (testing "requiring a dynamically created ns should not throw an exception"
    (is (= nil (let [a (call-ns-sym)] (require a))))))
