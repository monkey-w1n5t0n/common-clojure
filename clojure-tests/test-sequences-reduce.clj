(ns test-sequences-reduce
  (:require [clojure.test :refer :all]))

(deftest test-reduce-from-chunked-into-unchunked
  (is (= [1 2 \a \b] (into [] (concat [1 2] "ab")))))
