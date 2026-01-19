
(ns test-special-combined
  (:use clojure.test))

(deftest multiple-keys-in-destructuring
  (let [foo (fn [& {:keys [x]}] x)
        bar (fn [& options] (apply foo :x :b options))]
    (is (= (bar) :b))
    (is (= (bar :x :a) :a))))

(deftest empty-list-with-:as-destructuring
  (let [{:as x} '()]
    (is (= {} x))))

(deftest keywords-in-destructuring
  (let [m {:a 1 :b 2}]
    (let [{:keys [:a :b]} m]
      (is (= [1 2] [a b])))
    (let [{:keys [:a :b :c] :or {c 3}} m]
      (is (= [1 2 3] [a b c])))))

(deftest namespaced-keywords-in-destructuring
  (let [m {:a/b 1 :c/d 2}]
    (let [{:keys [:a/b :c/d]} m]
      (is (= [1 2] [b d])))))
