(ns test-with-out-str-2
  (:use clojure.test))

(deftest test-with-out-str-simple
  (is (string? (with-out-str (+ 1 2)))))
