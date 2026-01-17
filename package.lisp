(defpackage #:cl-clojure-syntax
  (:use #:cl)
  (:export #:enable-clojure-syntax
           #:disable-clojure-syntax
           #:*clojure-readtable*
           #:ensure-clojure-readtable
           #:read-clojure
           #:read-clojure-string
           #:preprocess-clojure-dots))

(defpackage #:cl-clojure-eval
  (:use #:cl)
  (:import-from #:cl-clojure-syntax
                #:enable-clojure-syntax
                #:*clojure-readtable*
                #:ensure-clojure-readtable
                #:read-clojure
                #:read-clojure-string
                #:preprocess-clojure-dots)
  (:export #:clojure-eval
           #:eval-string
           #:eval-file
           #:init-eval-system
           #:reset-env
           #:*current-env*
           #:*current-ns*
           #:make-env
           #:env-vars
           #:env-bindings
           #:env-parent
           #:make-var
           #:var-name
           #:var-namespace
           #:var-value
           #:var-metadata
           #:var-dynamic
           #:make-closure
           #:closure-params
           #:closure-body
           #:closure-env
           #:closure-name
           #:truthy?
           #:falsey?
           #:env-get-var
           #:env-intern-var
           #:env-set-var
           #:env-get-lexical
           #:env-extend-lexical
           #:env-push-bindings
           #:make-root-env
           #:setup-core-functions
           #:register-core-function
           #:apply-function
           #:eval-if
           #:eval-do
           #:eval-quote
           #:eval-var-quote
           #:eval-def
           #:eval-fn
           #:eval-defn
           #:eval-let
           #:eval-loop))
