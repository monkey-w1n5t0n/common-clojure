(defun test-fn (&rest args)
  (if (null args)
      ""
      (apply #'concatenate 'string
             (mapcar (lambda (x)
                      (cond
                        (t
                         (let ((s (make-string-output-stream)))
                           (princ x s)
                           (get-output-stream-string s)))))
                args))))
