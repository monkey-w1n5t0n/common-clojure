(ns test-debug-flow)

;; Step 1: Just return nil from a macro
(defmacro step1 []
  nil)
(step1)

;; Step 2: Return a number
(defmacro step2 []
  42)
(step2)

;; Step 3: Return a string
(defmacro step3 []
  "hello")
(step3)

;; Step 4: Return a quoted number
(defmacro step4 []
  '(quote 42))
(step4)
