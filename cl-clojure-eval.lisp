;;;; Clojure Evaluation System for SBCL
;;;; This implements the evaluation layer for Clojure on SBCL.

(in-package #:cl-clojure-eval)

;;; ============================================================
;;; Environment and Var System
;;; ============================================================

;;; A Var represents a Clojure variable - it holds a name, value,
;;; and metadata. Vars can be bound dynamically or have root values.

(defstruct var
  "A Clojure Var - holds name, namespace, value, and metadata."
  name
  (namespace 'user)
  (value nil)
  (metadata nil)
  (dynamic nil))

(defstruct env
  "An evaluation environment - holds vars, lexical bindings, and parent env."
  (vars (make-hash-table :test 'equal))    ; global vars by [ns name]
  (bindings nil)                           ; lexical bindings (alist)
  (parent nil))                            ; parent environment for nesting

(defparameter *current-env* nil
  "The current evaluation environment.")

(defparameter *current-ns* 'user
  "The current namespace symbol.")

(defun make-root-env ()
  "Create the root environment with core bindings."
  (let ((env (make-env)))
    ;; Initialize with nil as a special value
    (setf (gethash '[user nil] (env-vars env)) (make-var :name 'nil :value nil))
    env))

(defun env-get-var (env name &optional (ns '*current-ns*))
  "Get a var from the environment. Returns NIL if not found."
  (let* ((ns-name (if (eq ns '*current-ns*) *current-ns* ns))
         (key (if (listp name) name (list ns-name name))))
    (or (gethash key (env-vars env))
        (when (env-parent env)
          (env-get-var (env-parent env) name ns)))))

(defun env-intern-var (env name &optional (ns '*current-ns*))
  "Intern a new var in the environment or return existing one."
  (let* ((ns-name (if (eq ns '*current-ns*) *current-ns* ns))
         (key (list ns-name name))
         (vars (env-vars env))
         (existing (gethash key vars)))
    (if existing
        existing
        (let ((new-var (make-var :name name :namespace ns-name)))
          (setf (gethash key vars) new-var)
          new-var))))

(defun env-set-var (env name value &optional (ns '*current-ns*))
  "Set the value of a var in the environment."
  (let ((var (env-intern-var env name ns)))
    (setf (var-value var) value)
    var))

(defun env-get-lexical (env name)
  "Get a lexical binding from the environment."
  (let ((binding (assoc name (env-bindings env) :test #'equal)))
    (if binding
        (cdr binding)
        (when (env-parent env)
          (env-get-lexical (env-parent env) name)))))

(defun env-push-bindings (env bindings)
  "Create a new env frame with additional lexical bindings."
  (make-env :vars (env-vars env)
            :bindings (append bindings (env-bindings env))
            :parent env))

(defun env-extend-lexical (env name value)
  "Extend environment with a single lexical binding."
  (env-push-bindings env (list (cons name value))))

;;; ============================================================
;;; Special Forms Implementation
;;; ============================================================

;;; Each special form is implemented as a function that takes
;;; the form to evaluate and the current environment.

(defun eval-if (form env)
  "Evaluate an if form: (if test then else?)"
  (destructuring-bind (if-sym test then &optional else) form
    (declare (ignore if-sym))
    (let ((test-result (clojure-eval test env)))
      (if (truthy? test-result)
          (clojure-eval then env)
          (if else
              (clojure-eval else env)
              nil)))))

(defun eval-do (form env)
  "Evaluate a do form: (do expr*) - returns last expression's value."
  (let ((forms (cdr form)))
    (if (null forms)
        nil
        (let ((last-expr (car (last forms))))
          ;; Evaluate all but last for side effects
          (dolist (expr (butlast forms))
            (clojure-eval expr env))
          ;; Return evaluation of last
          (clojure-eval last-expr env)))))

(defun eval-quote (form env)
  "Evaluate a quote form: (quote expr) - returns expr unevaluated."
  (declare (ignore env))
  (cadr form))

(defun eval-var-quote (form env)
  "Evaluate a var quote: #'var-name - returns the Var object."
  (declare (ignore env))
  (let ((name (cadr form)))
    (env-get-var *current-env* name)))

(defun eval-def (form env)
  "Evaluate a def form: (def name expr?) - create/intern a var."
  (let* ((name (cadr form))
         (value-expr (caddr form))
         (value (if value-expr
                   (clojure-eval value-expr env)
                   nil)))
    (env-set-var env name value)
    name))

(defun eval-fn (form env)
  "Evaluate a fn form: (fn name? [args] body+) - returns a closure."
  ;; Extract function parts
  (let* ((rest-form (cdr form))
         (has-name (and (not (null rest-form))
                        (not (vectorp (car rest-form)))))
         (name (if has-name (car rest-form) nil))
         (params (if has-name (cadr rest-form) (car rest-form)))
         (body (if has-name (cddr rest-form) (cdr rest-form))))
    (make-closure :params params :body body :env env :name name)))

(defun eval-defn (form env)
  "Evaluate a defn form: (defn name [args] body+) - def a function."
  ;; defn expands to (def name (fn [args] body+))
  (let* ((name (cadr form))
         (fn-expr `(def ,name (fn ,@(cddr form))))
         (result (clojure-eval fn-expr env)))
    result))

(defun eval-let (form env)
  "Evaluate a let form: (let [bindings] body+)"
  (let* ((bindings (cadr form))
         (body (cddr form))
         (new-env env))
    ;; Process bindings pairwise
    (loop for (name value-expr) on bindings by #'cddr
          while name
          do (let ((value (clojure-eval value-expr new-env)))
               (setf new-env (env-extend-lexical new-env name value))))
    ;; Evaluate body in new environment
    (if (null body)
        nil
        (let ((last-expr (car (last body))))
          (dolist (expr (butlast body))
            (clojure-eval expr new-env))
          (clojure-eval last-expr new-env)))))

(defun eval-loop (form env)
  "Evaluate a loop form: (loop [bindings] body+) - with recur support."
  ;; For now, loop is like let but recur needs special handling
  ;; We'll implement a simplified version first
  (let* ((bindings (cadr form))
         (body (cddr form))
         (new-env env))
    ;; Process bindings pairwise
    (loop for (name value-expr) on bindings by #'cddr
          while name
          do (let ((value (clojure-eval value-expr new-env)))
               (setf new-env (env-extend-lexical new-env name value))))
    ;; Evaluate body in new environment
    (if (null body)
        nil
        (let ((last-expr (car (last body))))
          (dolist (expr (butlast body))
            (clojure-eval expr new-env))
          ;; Handle recur specially in last expression
          (let ((result (clojure-eval last-expr new-env)))
            ;; TODO: Handle recur return value
            result)))))

;;; ============================================================
;;; Closure (function) representation
;;; ============================================================

(defstruct closure
  "A Clojure function closure - captures params, body, and environment."
  params
  body
  env
  (name nil))

;;; ============================================================
;;; Core Functions (built-ins)
;;; ============================================================

(defun register-core-function (env name fn)
  "Register a core function in the environment."
  (env-set-var env name fn))

(defun setup-core-functions (env)
  "Set up all core functions in the environment."

  ;; Arithmetic functions
  (register-core-function env '+ #'clojure+)
  (register-core-function env '- #'clojure-)
  (register-core-function env '* #'clojure*)
  (register-core-function env '/ #'clojure/)
  (register-core-function env 'inc #'clojure-inc)
  (register-core-function env 'dec #'clojure-dec)

  ;; Comparison functions
  (register-core-function env '= #'clojure=)
  (register-core-function env '< #'clojure<)
  (register-core-function env '> #'clojure>)
  (register-core-function env '<= #'clojure<=)
  (register-core-function env '>= #'clojure>=)

  ;; Collection functions
  (register-core-function env 'cons #'clojure-cons)
  (register-core-function env 'conj #'clojure-conj)
  (register-core-function env 'first #'clojure-first)
  (register-core-function env 'rest #'clojure-rest)
  (register-core-function env 'count #'clojure-count)
  (register-core-function env 'vec #'clojure-vec)
  (register-core-function env 'vector #'clojure-vector)
  (register-core-function env 'list #'clojure-list)
  (register-core-function env 'map #'clojure-map)
  (register-core-function env 'apply #'clojure-apply)

  ;; Predicate functions
  (register-core-function env 'nil? #'clojure-nil?)
  (register-core-function env 'symbol? #'clojure-symbol?)
  (register-core-function env 'keyword? #'clojure-keyword?)
  (register-core-function env 'number? #'clojure-number?)
  (register-core-function env 'fn? #'clojure-fn?)
  (register-core-function env 'vector? #'clojure-vector?)

  ;; Sequence functions
  (register-core-function env 'seq #'clojure-seq)
  (register-core-function env 'identity #'clojure-identity)

  env)

;;; Arithmetic implementations
(defun clojure+ (&rest args)
  "Add all arguments. Returns 0 if no args."
  (if (null args)
      0
      (reduce #'+ args)))

(defun clojure- (first-arg &rest args)
  "Subtract. Negate if one arg, subtract rest from first if multiple."
  (if (null args)
      (- first-arg)
      (reduce #'- args :initial-value first-arg)))

(defun clojure* (&rest args)
  "Multiply all arguments. Returns 1 if no args."
  (if (null args)
      1
      (reduce #'* args)))

(defun clojure/ (first-arg &rest args)
  "Divide. Inverse if one arg, divide first by rest if multiple."
  (if (null args)
      (/ 1 first-arg)
      (reduce #'/ args :initial-value first-arg)))

(defun clojure-inc (n) (1+ n))
(defun clojure-dec (n) (1- n))

;;; Comparison implementations
(defun clojure= (&rest args)
  "True if all args are equal."
  (or (null args)
      (null (cdr args))
      (apply #'equal args)))

(defun clojure< (x &rest args)
  "True if arguments are in strictly increasing order."
  (or (null args)
      (and (< x (car args))
           (apply #'< args))))

(defun clojure> (x &rest args)
  "True if arguments are in strictly decreasing order."
  (or (null args)
      (and (> x (car args))
           (apply #'> args))))

(defun clojure<= (x &rest args)
  "True if arguments are in non-decreasing order."
  (or (null args)
      (and (<= x (car args))
           (apply #'<= args))))

(defun clojure>= (x &rest args)
  "True if arguments are in non-increasing order."
  (or (null args)
      (and (>= x (car args))
           (apply #'>= args))))

;;; Collection functions
(defun clojure-cons (x seq)
  "Cons x onto seq."
  (cons x seq))

(defun clojure-conj (coll &rest xs)
  "Conjoin elements to collection. For lists, adds to front."
  (if (null xs)
      coll
      (if (listp coll)
          (append (reverse xs) coll)
          ;; For vectors, add to end
          (if (vectorp coll)
              (coerce (append (coerce coll 'list) xs) 'vector)
              ;; For other collections, just append
              (append (coerce coll 'list) xs)))))

(defun clojure-first (seq)
  "Return first element of sequence."
  (if (or (null seq) (and (consp seq) (null (car seq))))
      nil
      (car seq)))

(defun clojure-rest (seq)
  "Return rest of sequence."
  (if (or (null seq) (and (consp seq) (null (cdr seq))))
      '()
      (cdr seq)))

(defun clojure-count (coll)
  "Return number of items in collection."
  (cond
    ((null coll) 0)
    ((listp coll) (length coll))
    ((vectorp coll) (length coll))
    ((stringp coll) (length coll))
    (t 0)))

(defun clojure-vec (coll)
  "Create a vector from collection."
  (if (vectorp coll)
      coll
      (coerce coll 'vector)))

(defun clojure-vector (&rest args)
  "Create a vector from arguments."
  (coerce args 'vector))

(defun clojure-list (&rest args)
  "Create a list from arguments."
  args)

(defun clojure-map (fn-arg coll &rest colls)
  "Apply fn to each item in collection(s). Returns lazy sequence."
  (declare (ignore fn-arg coll colls))
  ;; TODO: Implement map function
  '())

(defun clojure-apply (fn-arg &rest args)
  "Apply fn to args with last arg being a list of args."
  (let ((all-but-last (butlast args))
        (last-arg (car (last args))))
    (apply fn-arg (append all-but-last last-arg))))

(defun clojure-seq (coll)
  "Return a sequence from collection."
  (when coll
    (if (listp coll)
        coll
        (coerce coll 'list))))

(defun clojure-identity (x) x)

;;; Predicate implementations
(defun clojure-nil? (x) (null x))
(defun clojure-symbol? (x) (symbolp x))
(defun clojure-keyword? (x) (keywordp x))
(defun clojure-number? (x) (numberp x))
(defun clojure-fn? (x) (closure-p x))
(defun clojure-vector? (x) (vectorp x))

;;; ============================================================
;;; Truthiness
;;; ============================================================

(defun truthy? (value)
  "In Clojure, only nil and false are falsy. Everything else is truthy."
  (not (or (null value) (eq value 'false))))

(defun falsey? (value)
  "In Clojure, only nil and false are falsy."
  (or (null value) (eq value 'false)))

;;; ============================================================
;;; Main Evaluation Function
;;; ============================================================

(defun clojure-eval (form &optional env)
  "Evaluate a Clojure form in the given environment.
   If no environment provided, use *current-env*."
  (let ((env (or env *current-env*)))
    (typecase form
      ;; Self-evaluating forms
      (null nil)
      (number form)
      (string form)
      (character form)
      ((and symbol (satisfies keywordp)) form)
      (boolean (if form t nil))

      ;; Symbol - look up in environment
      (symbol
       ;; Check lexical bindings first
       (or (env-get-lexical env form)
           ;; Then check vars
           (let ((var (env-get-var env form)))
             (if var
                 (var-value var)
                 (error "Undefined symbol: ~A" form)))))

      ;; List - evaluate as function call or special form
      (cons
       (let ((head (car form))
             (rest-form (cdr form)))
         (cond
           ;; Special forms
           (eq head 'if) (eval-if form env)
           (eq head 'do) (eval-do form env)
           (eq head 'quote) (eval-quote form env)
           (eq head 'var) (eval-var-quote form env)
           (eq head 'def) (eval-def form env)
           (eq head 'defn) (eval-defn form env)
           (eq head 'fn) (eval-fn form env)
           (eq head 'fn*) (eval-fn form env)
           (eq head 'let) (eval-let form env)
           (eq head 'loop) (eval-loop form env)

           ;; Function application
           (t
            (let ((fn-value (clojure-eval head env))
                  (args (mapcar #'(lambda (x) (clojure-eval x env))
                                rest-form)))
              (apply-function fn-value args))))))

      ;; Vector - evaluate elements (for some contexts)
      (vector form)

      ;; Hash table - evaluate as map literal
      (hash-table form)

      ;; Default
      (t form))))

(defun apply-function (fn-value args)
  "Apply a function value to arguments."
  (typecase fn-value
    ;; Clojure closure
    (closure
     (let* ((params (closure-params fn-value))
            (body (closure-body fn-value))
            (fn-env (closure-env fn-value))
            ;; Create new environment with bindings
            (new-env fn-env))
       ;; Bind parameters to arguments
       (cond
         ;; Vector parameters - fixed arity
         (vectorp params)
         (loop for param across params
               for arg in args
               do (setf new-env (env-extend-lexical new-env param arg)))
         ;; Handle & rest params
         (when (and (> (length params) 1)
                    (find '& params))
           (let ((&-pos (position '& params))
                 (rest-params (subseq params (1+ (position '& params))))
                 (rest-args (nthcdr (length (subseq params 0 (position '& params))) args)))
             ;; Bind rest param
             (setf new-env (env-extend-lexical new-env
                                               (car rest-params)
                                               rest-args))))
         ;; List parameters
         (t
          (loop for param in params
                for arg in args
                do (setf new-env (env-extend-lexical new-env param arg)))))

       ;; Evaluate body
       (if (null body)
           nil
           (let ((last-expr (car (last body))))
             (dolist (expr (butlast body))
               (clojure-eval expr new-env))
             (clojure-eval last-expr new-env)))))

    ;; Regular Lisp function
    (function (apply fn-value args))

    ;; Error
    (t (error "Cannot apply non-function: ~A" fn-value))))

;;; ============================================================
;;; REPL and Evaluation Interface
;;; ============================================================

(defun init-eval-system ()
  "Initialize the evaluation system with root environment."
  (setf *current-env* (make-root-env))
  (setup-core-functions *current-env*)
  *current-env*)

(defun eval-string (string)
  "Evaluate a Clojure string and return the result."
  (unless *current-env*
    (init-eval-system))
  (cl-clojure-syntax:enable-clojure-syntax)
  (let* ((form (cl-clojure-syntax:read-clojure-string string)))
    (clojure-eval form *current-env*)))

(defun eval-file (path)
  "Evaluate all forms in a Clojure file."
  (unless *current-env*
    (init-eval-system))
  (let ((content (with-open-file (s path :direction :input)
                   (let ((str (make-string (file-length s))))
                     (read-sequence str s)
                     str)))
        (preprocessed (cl-clojure-syntax:preprocess-clojure-dots content))
        (results nil))
    (with-input-from-string (stream preprocessed)
      (let ((*readtable* (cl-clojure-syntax:ensure-clojure-readtable)))
        (loop for form = (cl-clojure-syntax:read-clojure stream nil :eof)
              until (eq form :eof)
              do (push (clojure-eval form *current-env*) results))))
    (nreverse results)))

(defun reset-env ()
  "Reset the evaluation environment."
  (setf *current-env* (make-root-env))
  (setup-core-functions *current-env*)
  *current-env*)
