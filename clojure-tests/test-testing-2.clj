(ns test-testing-2
  (:use clojure.test))

(deftest test-simple
  (testing "a test"
    (is (= 1 1))))
