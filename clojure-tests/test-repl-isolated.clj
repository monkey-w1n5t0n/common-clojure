(ns test-repl-isolated
  (:use clojure.test
        clojure.repl))

(deftest test-doc-simple
  (is (string? (with-out-str (doc catch)))))

(deftest test-source-simple
  (is (nil? (source-fn 'non-existent-fn))))

(deftest test-dir-simple
  (is (= '() (dir-fn 'user))))

(deftest test-apropos-simple
  (is (vector? (apropos #"nothing-has-this-name"))))
