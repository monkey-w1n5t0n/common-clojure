;;;; Clojure Transducers for SBCL
;;;; This file contains transducer support that extends the base clojure eval system
;;;; Load this after cl-clojure-eval.lisp

(in-package :cl-clojure-eval)

;; Note: +transducer-sentinel+ is defined in cl-clojure-eval.lisp

;; Override clojure-map to support transducer arity
(defun clojure-map (fn-arg &optional (coll +transducer-sentinel+) &rest colls)
  "Apply fn to each item in collection(s). Returns lazy sequence.
   When called with only fn, returns a transducer."
  (let ((callable-fn (ensure-callable fn-arg)))
    (if (eq coll +transducer-sentinel+)
        ;; Transducer arity - (map f) returns a transducer
        ;; Note: Sentinel value distinguishes (map f) from (map f nil)
        (lambda (reducing-function)
          "Map transducer - applies f to each input before reducing."
          (lambda (result input)
            (funcall reducing-function result (funcall callable-fn input))))
        ;; Collection arity - map over collection(s)
        (if (null colls)
            ;; Single collection mapping
            (cond
              ((null coll) '())
              ((lazy-range-p coll)
               (let ((start (lazy-range-start coll))
                     (end (lazy-range-end coll))
                     (step (lazy-range-step coll)))
                 (if end
                     (loop for i from start below end by step
                           collect (funcall callable-fn i))
                     (loop for i from start by step
                           repeat 1000
                           collect (funcall callable-fn i)))))
              ((listp coll)
               (mapcar callable-fn coll))
              ((hash-table-p coll)
               (let ((result '()))
                 (maphash (lambda (k v)
                            (push (vector k v) result))
                          coll)
                 (mapcar callable-fn (nreverse result))))
              ((vectorp coll)
               (mapcar callable-fn (coerce coll 'list)))
              ((stringp coll)
               (mapcar callable-fn (coerce coll 'list)))
              (t
               (mapcar callable-fn (coerce coll 'list))))
            ;; Multiple collections - map in parallel
            (let* ((all-colls (cons coll colls))
                   (coll-length (lambda (c)
                                  (cond
                                    ((null c) 0)
                                    ((lazy-range-p c)
                                     (if (lazy-range-end c)
                                         (ceiling (/ (- (lazy-range-end c)
                                                        (lazy-range-start c))
                                                     (lazy-range-step c)))
                                         1000))
                                    ((listp c) (length c))
                                    ((hash-table-p c)
                                     (hash-table-count c))
                                    ((vectorp c) (length c))
                                    ((stringp c) (length c))
                                    (t (length (coerce c 'list))))))
                   (min-length (apply #'min (mapcar coll-length all-colls))))
              (let ((coll-lists (mapcar (lambda (c)
                                          (cond
                                            ((null c) '())
                                            ((lazy-range-p c)
                                             (lazy-range-to-list c (if (lazy-range-end c)
                                                                       most-positive-fixnum
                                                                       10000)))
                                            ((listp c) c)
                                            ((hash-table-p c)
                                             (let ((result '()))
                                               (maphash (lambda (k v)
                                                          (push (vector k v) result))
                                                        c)
                                               (nreverse result)))
                                            ((vectorp c)
                                             (coerce c 'list))
                                            ((stringp c)
                                             (coerce c 'list))
                                            (t (coerce c 'list))))
                                    all-colls)))
                (loop for i from 0 below min-length
                      collect (apply callable-fn (mapcar (lambda (c) (nth i c)) coll-lists)))))))))

;; Override clojure-filter to support transducer arity
(defun clojure-filter (pred &optional (coll +transducer-sentinel+))
  "Return a lazy sequence of items in coll for which pred returns true.
   When called with only pred, returns a transducer."
  (let ((callable-pred (ensure-callable pred)))
    (if (eq coll +transducer-sentinel+)
        ;; Transducer arity - (filter pred) returns a transducer
        (lambda (reducing-function)
          "Filter transducer - only includes inputs where pred returns true."
          (lambda (result input)
            (if (funcall callable-pred input)
                (funcall reducing-function result input)
                result)))
        ;; Collection arity - filter over collection (even if coll is nil)
        (cond
          ((null coll) '())
          ((lazy-range-p coll)
           (let ((start (lazy-range-start coll))
                 (end (lazy-range-end coll))
                 (step (lazy-range-step coll)))
             (if end
                 (loop for i from start below end by step
                       when (funcall callable-pred i)
                       collect i)
                 (loop for i from start by step
                       repeat 1000
                       when (funcall callable-pred i)
                       collect i))))
          ((listp coll)
           (loop for item in coll
                 when (funcall callable-pred item)
                 collect item))
          ((hash-table-p coll)
           (let ((result '()))
             (maphash (lambda (k v)
                        (when (funcall callable-pred (vector k v))
                          (push (vector k v) result)))
                      coll)
             (nreverse result)))
          ((vectorp coll)
           (let ((coll-list (coerce coll 'list)))
             (loop for item in coll-list
                   when (funcall callable-pred item)
                   collect item)))
          (t
           (let ((coll-list (coerce coll 'list)))
             (loop for item in coll-list
                   when (funcall callable-pred item)
                   collect item)))))))

;; Cat transducer - concatenates collections during transduction
(defun clojure-cat (&optional (coll +transducer-sentinel+))
  "Concatenation transducer. When called with no args, returns the cat transducer.
   When called with a collection, returns that collection converted to a list."
  (if (eq coll +transducer-sentinel+)
      ;; Transducer arity - (cat) returns a transducer
      (lambda (reducing-function)
        "Cat transducer - concatenates input collections."
        (lambda (result input)
          ;; input is expected to be a collection, concatenate it to result
          (let ((input-list (cond
                             ((null input) '())
                             ((lazy-range-p input)
                              (lazy-range-to-list input (if (lazy-range-end input)
                                                          most-positive-fixnum
                                                          10000)))
                             ((listp input) input)
                             ((hash-table-p input)
                              (let ((ht-result '()))
                                (maphash (lambda (k v)
                                           (push (vector k v) ht-result))
                                         input)
                                (nreverse ht-result)))
                             ((vectorp input)
                              (coerce input 'list))
                             ((stringp input)
                              (coerce input 'list))
                             (t (coerce input 'list)))))
            (append result input-list))))
      ;; Collection arity - just return the collection as a list
      (cond
        ((null coll) '())
        ((lazy-range-p coll)
         (lazy-range-to-list coll (if (lazy-range-end coll)
                                     most-positive-fixnum
                                     10000)))
        ((listp coll) coll)
        ((hash-table-p coll)
         (let ((ht-result '()))
           (maphash (lambda (k v)
                      (push (vector k v) ht-result))
                    coll)
           (nreverse ht-result)))
        ((vectorp coll)
         (coerce coll 'list))
        ((stringp coll)
         (coerce coll 'list))
        (t (coerce coll 'list)))))

;; Transduce function
(defun clojure-transduce (xform f &optional init coll)
  "Transduce a collection using a transducer.
   (transduce xform f) - returns reducing function
   (transduce xform f coll) - reduce with no init
   (transduce xform f init coll) - reduce with init value"
  (let ((callable-xform (if (functionp xform) xform (ensure-callable xform)))
        (callable-f (ensure-callable f)))
    (cond
      ;; No coll provided - return the reducing function
      ((null coll)
       (if (null init)
           (funcall callable-xform callable-f)
           (funcall callable-xform (lambda (&rest args)
                                   (apply callable-f args)))))
      ;; No init value provided - reduce with first element as init
      ((null init)
       (let ((reduced (funcall callable-xform callable-f)))
         (let ((coll-list (cond
                           ((null coll) '())
                           ((lazy-range-p coll)
                            (lazy-range-to-list coll (if (lazy-range-end coll)
                                                        most-positive-fixnum
                                                        10000)))
                           ((listp coll) coll)
                           ((hash-table-p coll)
                            (let ((ht-result '()))
                              (maphash (lambda (k v)
                                         (push (vector k v) ht-result))
                                       coll)
                              (nreverse ht-result)))
                           ((vectorp coll)
                            (coerce coll 'list))
                           ((stringp coll)
                            (coerce coll 'list))
                           (t (coerce coll 'list)))))
           (cond
             ((null coll-list)
              ;; Empty collection with no init - call f with no args
              (funcall callable-f))
             ((null (cdr coll-list))
              ;; Single element - just return f applied to it
              (funcall callable-f (car coll-list)))
             (t
              ;; Multiple elements - reduce
              (reduce (lambda (result item)
                       (funcall reduced result item))
                      (cdr coll-list)
                      :initial-value (car coll-list)))))))
      ;; With init value
      (t
       (let ((reduced (funcall callable-xform callable-f)))
         (let ((coll-list (cond
                           ((null coll) '())
                           ((lazy-range-p coll)
                            (lazy-range-to-list coll (if (lazy-range-end coll)
                                                        most-positive-fixnum
                                                        10000)))
                           ((listp coll) coll)
                           ((hash-table-p coll)
                            (let ((ht-result '()))
                              (maphash (lambda (k v)
                                         (push (vector k v) ht-result))
                                       coll)
                              (nreverse ht-result)))
                           ((vectorp coll)
                            (coerce coll 'list))
                           ((stringp coll)
                            (coerce coll 'list))
                           (t (coerce coll 'list)))))
           (if (null coll-list)
               init
               (reduce (lambda (result item)
                        (funcall reduced result item))
                      coll-list
                      :initial-value init))))))))
(defun clojure-dedupe (&optional (coll +transducer-sentinel+))
  "Remove consecutive duplicates from collection."
  (if (eq coll +transducer-sentinel+)
      (lambda (reducing-function)
        (let ((previous nil)
              (first-p t))
          (lambda (result input)
            (if (or first-p (not (equal input previous)))
                (progn
                  (setf previous input)
                  (setf first-p nil)
                  (funcall reducing-function result input))
                result))))
      (let ((result '())
            (previous nil)
            (first-p t))
        (dolist (item (if (listp coll) coll (coerce coll 'list)))
          (if (or first-p (not (equal item previous)))
              (push item result))
          (setf previous item)
          (setf first-p nil))
        (nreverse result))))

;; Take-nth transducer
(defun clojure-take-nth (n &optional (coll +transducer-sentinel+))
  "Return every nth element."
  (if (eq coll +transducer-sentinel+)
      (let ((index 0))
        (lambda (reducing-function)
          (lambda (result input)
            (let ((current-index index))
              (incf index)
              (if (= 0 (mod current-index n))
                  (funcall reducing-function result input)
                  result)))))
      (loop for i from 0
            for elem in (if (listp coll) coll (coerce coll 'list))
            when (= 0 (mod i n))
            collect elem)))

;; Replace transducer
(defun clojure-replace (smap &optional (coll +transducer-sentinel+))
  "Replace values in coll using smap (a map of from -> to).
   When called with only smap, returns a transducer."
  (if (eq coll +transducer-sentinel+)
      ;; Transducer arity
      (lambda (reducing-function)
        (lambda (result input)
          (funcall reducing-function result
                  (let ((val (gethash input smap)))
                    (if val val input)))))
      ;; Collection arity
      (let ((result '()))
        (dolist (item (if (listp coll) coll (coerce coll 'list)))
          (push (let ((val (gethash item smap)))
                   (if val val item))
                result))
        (nreverse result))))

;; Interpose transducer
(defun clojure-interpose (sep &optional (coll +transducer-sentinel+))
  "Insert sep between elements of coll.
   When called with only sep, returns a transducer."
  (if (eq coll +transducer-sentinel+)
      ;; Transducer arity
      (let ((first-p t))
        (lambda (reducing-function)
          (lambda (result input)
            (if first-p
                (progn
                  (setf first-p nil)
                  (funcall reducing-function result input))
                (funcall reducing-function
                          (funcall reducing-function result sep)
                          input)))))
      ;; Collection arity
      (when (and coll (not (null coll)))
        (let ((result '()))
          (let ((items (if (listp coll) coll (coerce coll 'list))))
            (when items
              (push (car items) result)
              (dolist (item (cdr items))
                (push sep result)
                (push item result))))
          (nreverse result)))))

;; Keep-indexed transducer
(defun clojure-keep-indexed (f &optional (coll +transducer-sentinel+))
  "Keep indices and items where (f index item) returns truthy.
   When called with only f, returns a transducer."
  (let ((callable-f (ensure-callable f)))
    (if (eq coll +transducer-sentinel+)
        ;; Transducer arity
        (let ((index -1))
          (lambda (reducing-function)
            (lambda (result input)
              (incf index)
              (let ((val (funcall callable-f index input)))
                (if val
                    (funcall reducing-function result val)
                    result)))))
        ;; Collection arity
        (let ((result '())
              (index -1))
          (dolist (item (if (listp coll) coll (coerce coll 'list)))
            (incf index)
            (let ((val (funcall callable-f index item)))
              (when val
                (push val result))))
          (nreverse result)))))

;; Map-indexed transducer
(defun clojure-map-indexed (f &optional (coll +transducer-sentinel+))
  "Map (f index item) over collection.
   When called with only f, returns a transducer."
  (let ((callable-f (ensure-callable f)))
    (if (eq coll +transducer-sentinel+)
        ;; Transducer arity
        (let ((index -1))
          (lambda (reducing-function)
            (lambda (result input)
              (incf index)
              (funcall reducing-function result
                      (funcall callable-f index input)))))
        ;; Collection arity
        (let ((result '())
              (index -1))
          (dolist (item (if (listp coll) coll (coerce coll 'list)))
            (incf index)
            (push (funcall callable-f index item) result))
          (nreverse result)))))

;; Sequence with transducer support
(defun clojure-sequence-xform (xform &rest colls)
  "Apply transducer xform to collection(s) and return lazy sequence."
  (let ((reducer (funcall xform #'cons)))
    (if (null colls)
        '()
        (let* ((coll (car colls))
               (items (if (listp coll) coll (coerce coll 'list))))
          (reduce reducer items :initial-value '())))))

;; Register transducer functions
(defun setup-transducer-functions (env)
  "Register transducer functions in the environment."
  ;; Register overridden functions
  (register-core-function env 'map #'clojure-map)
  (register-core-function env 'filter #'clojure-filter)
  ;; Register new transducer functions
  (register-core-function env 'cat #'clojure-cat)
  (register-core-function env 'transduce #'clojure-transduce)
  (register-core-function env 'dedupe #'clojure-dedupe)
  (register-core-function env 'take-nth #'clojure-take-nth)
  (register-core-function env 'replace #'clojure-replace)
  (register-core-function env 'interpose #'clojure-interpose)
  (register-core-function env 'keep-indexed #'clojure-keep-indexed)
  (register-core-function env 'map-indexed #'clojure-map-indexed)
  (register-core-function env 'sequence-xform #'clojure-sequence-xform))
