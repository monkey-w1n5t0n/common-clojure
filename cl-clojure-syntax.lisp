(in-package #:cl-clojure-syntax)

;;; Clojure uses a specialized reader that preserves case.
;;; We create a separate readtable for Clojure code instead of modifying
;;; the global one, since :preserve mode breaks Common Lisp's t/nil.

(defvar *clojure-readtable* nil
  "The readtable used for reading Clojure code.")

(defun ensure-clojure-readtable ()
  "Create the Clojure readtable if it doesn't exist."
  (unless *clojure-readtable*
    (setf *clojure-readtable* (copy-readtable nil))
    ;; Set readtable case to :preserve for Clojure compatibility
    (setf (readtable-case *clojure-readtable*) :preserve)

    ;; [ ]
    (set-macro-character #\[ #'read-vector nil *clojure-readtable*)
    (set-macro-character #\] #'right-delimiter-reader nil *clojure-readtable*)

    ;; { }
    (set-macro-character #\{ #'read-map nil *clojure-readtable*)
    (set-macro-character #\} #'right-delimiter-reader nil *clojure-readtable*)

    ;; ::foo - auto-resolved keyword
    (set-macro-character #\: #'read-colon-dispatch nil *clojure-readtable*))
  *clojure-readtable*)

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

;;; Auto-resolved keyword reader for ::foo syntax
;;; This reads :foo and resolves it to :current-namespace/foo
(defun read-auto-resolved-keyword (stream char)
  (declare (ignore char))
  ;; Read the next token (the keyword name without the prefix :)
  (let* ((token (read stream t nil t))
         ;; For now, we just prepend a default namespace
         ;; In a full implementation, this would resolve against *ns*
         (ns-name "user")
         (kw-name (if (keywordp token)
                      (symbol-name token)
                      (princ-to-string token))))
    (intern (concatenate 'string ns-name "/" kw-name) :keyword)))

(defun read-colon-dispatch (stream char)
  "Dispatch for : character. If followed by :, it's auto-resolved. Otherwise normal keyword."
  (declare (ignore char))
  (let ((next-char (peek-char nil stream nil nil)))
    (if (eql next-char #\:)
        ;; Consume the second : and read the auto-resolved keyword
        (progn
          (read-char stream)  ; consume the second :
          (read-auto-resolved-keyword stream #\:))
        ;; Normal keyword - let default reader handle it
        (let ((token (read stream t nil t)))
          (if (symbolp token)
              (intern (symbol-name token) :keyword)
              token)))))

(defun enable-clojure-syntax ()
  "Enable Clojure syntax for subsequent read operations."
  (ensure-clojure-readtable)
  (setq *readtable* *clojure-readtable*))

(defun disable-clojure-syntax ()
  "Disable Clojure syntax, restoring default Common Lisp readtable."
  ;; Restore default readtable (SBCL's standard readtable)
  (setq *readtable* (copy-readtable nil)))

