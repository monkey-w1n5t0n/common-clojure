(ns test-repl-parts
  (:use clojure.test
        clojure.repl))

;; Test doc
(deftest test-doc-simple
  (is (string? (with-out-str (doc catch)))))

;; Test source
(deftest test-source-simple
  (is (nil? (source-fn 'non-existent-fn))))

;; Test dir
(deftest test-dir-simple
  (is (= '() (dir-fn 'user))))

;; Test apropos
(deftest test-apropos-simple
  (is (vector? (apropos #"nothing-has-this-name"))))
