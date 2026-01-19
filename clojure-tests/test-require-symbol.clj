(ns test-require-symbol
  (:use clojure.test))

(deftest test-require-symbol
  (is (= nil (let [a 'user] (require a)))))
