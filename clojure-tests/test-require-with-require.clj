(ns test-require-with-require
  (:use clojure.test)
  (:require [clojure.string :as str]))

(deftest test-simple-require
  (is (= 1 1)))
