
(ns test-special-ns-keys
  (:use clojure.test))

(deftest namespaced-keys-syntax
  (let [{:a/keys [b c d] :or {d 3}} {:a/b 1 :a/c 2}]
    (is (= [1 2 3] [b c d]))))
