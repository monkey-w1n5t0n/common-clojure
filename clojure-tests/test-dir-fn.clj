(ns test-dir-fn
  (:use clojure.test
        clojure.repl))

(deftest test-dir-fn-throws
  (is (thrown? Exception (dir-fn 'non-existent-ns))))

(deftest test-dir-fn-returns-list
  (is (= '() (dir-fn 'user))))
