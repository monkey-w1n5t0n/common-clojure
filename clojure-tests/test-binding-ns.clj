(ns test-binding-ns
  (:use clojure.test))

(deftest test-binding-ns
  (binding [*ns* (the-ns 'test-binding-ns)]
    (is (= 'test-binding-ns *ns*))))
