;   Copyright (c) Rich Hickey. All rights reserved.

(ns clojure.test-clojure.special
  (:use clojure.test)
  (:require [clojure.test-helper :refer [should-not-reflect]]))

(deftest namespaced-keywords-in-destructuring
  (let [m {:a/b 1 :c/d 2}]
    (let [{:keys [:a/b :c/d]} m]
      (is (= [1 2] [b d])))))
