;; Test for PersistentList/EMPTY
(ns test-persistent-list)

(deftest test-empty
  (is (= '() '()))
  (is (identical? '() (.empty (seq [1 2 3])))))

;; Test from vectors.clj
(deftest test-are-empty
  (are [a] (identical? '() (.empty (seq a)))
       [1 2 3]
       '(1 2 3)
       ()))

;; Test .cons
(deftest test-cons
  (is (= '(:foo 1) (.cons (seq (into (vector-of :int) [1])) :foo))))

;; Test .. (dot-dot)
(deftest test-dot-dot
  (let [v [1 2 3]]
    (is (= 3 (.count v)))
    (is (= [1 2 3] v))))

;; Test into and vector-of
(deftest test-into-vector-of
  (let [r (range 6)
        v (into (vector-of :int) r)]
    (is (= 6 (.count v)))
    (is (vector? v))))

;; Test rseq
(deftest test-rseq
  (let [v (into (vector-of :int) (range 6))
        reversed (.rseq v)]
    (is (= '(5 4 3 2 1 0) reversed))
    (is (= 5 (.index reversed)))
    (is (= 5 (.first reversed)))))

;; Test class comparison
(deftest test-class-comparison
  (let [v [1 2 3]]
    (is (= 'clojure.lang.PersistentVector (class v)))))

;; Test APersistentVector$RSeq symbol
(deftest test-rseq-symbol
  (is (symbol? 'clojure.lang.APersistentVector$RSeq)))
