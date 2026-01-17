(defparameter depth 0)
(defparameter line-nr 0)
(defparameter col-nr 0)

(with-open-file (f "cl-clojure-eval.lisp")
  (loop for line = (read-line f nil nil)
        while line
        do (incf line-nr)
           (setf col-nr 0)
           (when (>= line-nr 1047)
             (loop for c across line
                   do (incf col-nr)
                      (case c
                        (#\( (incf depth))
                        (#\) (decf depth))))
             (when (and (> line-nr 1520) (< line-nr 1540))
               (format t "Line ~d: depth ~d~%" line-nr depth))
             (when (< depth -1)
               (format t "Line ~d, Col ~d: Extra ) at depth ~d~%" line-nr col-nr depth)
               (return-from nil))
             (when (= line-nr 1540)
               (format t "Line ~d: Final depth ~d~%" (1- line-nr) depth)
               (return)))))

(format t "Done.~%")
