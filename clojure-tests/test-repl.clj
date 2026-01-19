(ns test-repl
  (:use clojure.test
        clojure.repl))

(deftest test-dir-simple
  (is (= '() (dir-fn 'user))))
