;; Test keyword lookup with nil
(load "package.lisp")
(load "cl-clojure-syntax.lisp")
(load "cl-clojure-eval.lisp")

(init-eval-system)

;; Test (:keyword nil) pattern
(let ((result (clojure-eval '(:arglists nil) *current-env*)))
  (format t "(:arglists nil) = ~A~%" result))

;; Test (count nil)
(let ((result (clojure-eval '(count nil) *current-env*)))
  (format t "(count nil) = ~A~%" result))

;; Test combined
(let ((result (clojure-eval '(count (:arglists nil)) *current-env*)))
  (format t "(count (:arglists nil)) = ~A~%" result))

;; Test with empty map
(let ((result (clojure-eval '(count (:arglists {})) *current-env*)))
  (format t "(count (:arglists {})) = ~A~%" result))

;; Test > with nil
(let ((result (clojure-eval '(> 0 0) *current-env*)))
  (format t "(> 0 0) = ~A~%" result))
