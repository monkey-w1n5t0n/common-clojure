(in-package #:cl-clojure-syntax)

(defun read-vector (stream char)
  (declare (ignore char))
  (apply #'vector (read-delimited-list #\] stream t)))

(defun read-map (stream char)
  (declare (ignore char))
  (let ((items (read-delimited-list #\} stream t))
        (table (make-hash-table :test 'equal)))
    (unless (evenp (length items))
      (error "Map literal must have an even number of elements"))
    (loop for (key value) on items by #'cddr
          do (setf (gethash key table) value))
    table))

(defun right-delimiter-reader (stream char)
  (declare (ignore stream))
  (error "Unmatched closing delimiter ~A" char))

(defun enable-clojure-syntax ()
  ;; [ ]
  (set-macro-character #\[ #'read-vector)
  (set-macro-character #\] #'right-delimiter-reader)
  
  ;; { }
  (set-macro-character #\{ #'read-map)
  (set-macro-character #\} #'right-delimiter-reader))

(defun disable-clojure-syntax ()
  (set-macro-character #\[ nil)
  (set-macro-character #\] nil)
  (set-macro-character #\{ nil)
  (set-macro-character #\} nil))

