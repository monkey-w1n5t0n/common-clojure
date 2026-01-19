(ns test-testing
  (:use clojure.test))

(deftest test-with-testing
  (testing "simple test"
   (is (= nil nil))))
