(defpackage #:cl-clojure-syntax
  (:use #:cl)
  (:export #:enable-clojure-syntax
           #:disable-clojure-syntax
           #:*clojure-readtable*
           #:ensure-clojure-readtable
           #:read-clojure
           #:read-clojure-string
           #:preprocess-clojure-dots))
