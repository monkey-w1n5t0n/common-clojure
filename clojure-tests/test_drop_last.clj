(deftest test-drop-last-simple
  (let [nums (range 1 100)]
    (is (not (nil? (drop-last nums))))))
