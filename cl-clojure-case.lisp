;;;; Case special form for Clojure eval

(in-package #:cl-clojure-eval)

(defun eval-case (form env)
  "Evaluate a case form. Each clause is tested value compared to expr.
   Returns the result of the first matching test, or the default if no match."
  (let* ((expr-expr (cadr form))
         (clauses (cddr form))
         (expr-value (clojure-eval expr-expr env)))
    ;; Process clauses pairwise
    (loop for (test result) on clauses by (function cddr)
          when (null test)
            ;; Odd number of clauses - last is default
            do (return-from eval-case (clojure-eval result env))
          when (equal expr-value (clojure-eval test env))
            ;; Found a match - return the result
            do (return-from eval-case (clojure-eval result env))
          ;; No match, continue to next clause
          finally (return-from eval-case nil))))
