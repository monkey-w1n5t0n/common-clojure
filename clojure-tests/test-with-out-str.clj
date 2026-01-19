(ns test-with-out-str
  (:use clojure.test))

(deftest test-with-out-str-simple
  (is (string? (with-out-str (print "hello")))))
