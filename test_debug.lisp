(in-package #:clojure-test-runner)

(clojure-eval '(count (:arglists nil)) clojure-eval::*current-env*)
