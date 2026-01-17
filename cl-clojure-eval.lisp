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
  (let ((env (make-env :vars (make-hash-table :test 'equal))))
    ;; Initialize with nil as a special value
    (setf (gethash "USER/NIL" (env-vars env))
          (make-var :name 'nil :namespace 'user :value nil))
    env))

(defun var-key (name &optional (ns '*current-ns*))
  "Create a string key for var lookup, independent of symbol package and case.
   Clojure symbols are case-sensitive but stored in uppercase for consistency."
  (let ((ns-name (if (eq ns '*current-ns*) *current-ns* ns)))
    (concatenate 'string (string-upcase (string ns-name)) "/" (string-upcase (string name)))))

(defun env-get-var (env name &optional (ns '*current-ns*))
  "Get a var from the environment. Returns NIL if not found.
   Falls back to 'user' namespace if not found in current namespace."
  (let* ((ns-name (if (eq ns '*current-ns*) *current-ns* ns))
         (key (var-key name ns-name))
         (result (or (gethash key (env-vars env))
                     (when (env-parent env)
                       (env-get-var (env-parent env) name ns-name)))))
    ;; If not found in current namespace, try the 'user' namespace (core)
    (or result
        (when (and (not (eq ns-name 'user))
                   (not (env-parent env)))  ; Only check at root level
          (gethash (var-key name 'user) (env-vars env)))
        (when (env-parent env)
          (env-get-var (env-parent env) name 'user)))))

(defun env-intern-var (env name &optional (ns '*current-ns*))
  "Intern a new var in the environment or return existing one."
  (let* ((ns-name (if (eq ns '*current-ns*) *current-ns* ns))
         (key (var-key name ns-name))
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
  ;; For lexical bindings, use string comparison for symbol names
  ;; to handle symbols from different packages
  ;; We manually search because assoc's :key would need to handle both
  ;; the lookup value and list elements, which have different types
  (let ((name-string (string name)))
    (labels ((search-bindings (bindings)
               (cond
                 ((null bindings) nil)
                 ((string= name-string (string (caar bindings)))
                  (car bindings))
                 (t (search-bindings (cdr bindings))))))
      (let ((binding (search-bindings (env-bindings env))))
        (if binding
            (cdr binding)
            (when (env-parent env)
              (env-get-lexical (env-parent env) name)))))))

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

(defun eval-syntax-quote (form env)
  "Evaluate a syntax-quote form: `(syntax-quote form) - like quote but with unquote support."
  ;; Syntax-quote returns its form, but processes any nested unquote/unquote-splicing
  (let ((form-to-quote (cadr form)))
    ;; Process the form, handling unquote and unquote-splicing
    (process-syntax-quote form-to-quote env)))

(defun process-syntax-quote (form env)
  "Process a syntax-quote form, expanding unquote and unquote-splicing."
  (typecase form
    ;; Handle unquote - evaluate and return the value
    (cons
     (if (eq (car form) 'unquote)
         ;; Evaluate the unquoted form in the current environment
         (clojure-eval (cadr form) env)
         ;; Handle unquote-splicing
         (if (eq (car form) 'unquote-splicing)
             ;; Evaluate and return the value (should be a list for splicing)
             (clojure-eval (cadr form) env)
             ;; Regular list - recursively process elements and build list
             (let ((processed (mapcar (lambda (x) (process-syntax-quote x env)) form)))
               processed))))
    ;; For vectors, process each element and return as vector
    (vector
     (let ((processed (mapcar (lambda (x) (process-syntax-quote x env)) (coerce form 'list))))
       (coerce processed 'vector)))
    ;; For symbols, return them as-is (will be resolved when final code is evaluated)
    ;; In a real implementation, symbols would be namespace-qualified
    (symbol form)
    ;; Everything else just return as-is
    (t form)))

(defun eval-unquote (form env)
  "Evaluate an unquote form: (unquote form) - should only appear within syntax-quote."
  ;; Unquote is handled during syntax-quote processing
  ;; If we reach here, it's an unquote outside syntax-quote, which is an error
  (error "Unquote outside syntax-quote"))

(defun eval-unquote-splicing (form env)
  "Evaluate an unquote-splicing form: (unquote-splicing form) - should only appear within syntax-quote."
  ;; Unquote-splicing is handled during syntax-quote processing
  (error "Unquote-splicing outside syntax-quote"))

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

(defun eval-defmacro (form env)
  "Evaluate a defmacro form: (defmacro name [args] body+) - def a macro."
  ;; defmacro creates a macro function - similar to defn but marked as macro
  (let* ((name (cadr form))
         ;; Create the macro closure with macro-p flag set
         (rest-form (cddr form))
         (has-name (and (not (null rest-form))
                        (not (vectorp (car rest-form)))))
         (macro-name (if has-name (car rest-form) nil))
         (params (if has-name (cadr rest-form) (car rest-form)))
         (body (if has-name (cddr rest-form) (cdr rest-form)))
         (macro (make-closure :params params :body body :env env :name macro-name :macro-p t)))
    ;; Store the macro in the environment
    (env-set-var env name macro)
    name))

(defun eval-let (form env)
  "Evaluate a let form: (let [bindings] body+)"
  (let* ((bindings (cadr form))
         (body (cddr form))
         (new-env env))
    ;; Convert vector bindings to list for iteration
    (let ((bindings-list (if (vectorp bindings)
                             (coerce bindings 'list)
                             bindings)))
      ;; Process bindings pairwise
      (loop for (name value-expr) on bindings-list by #'cddr
            while name
            do (let ((value (clojure-eval value-expr new-env)))
                 (setf new-env (env-extend-lexical new-env name value)))))
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

(defun eval-ns (form env)
  "Evaluate a ns form: (ns name & options) - set current namespace."
  ;; For now, just set the current namespace and make test macros available
  ;; TODO: Handle :use, :import, :require, etc.
  (let* ((name (cadr form))
         ;; Extract the name part from a qualified symbol like clojure.test-clojure.agents
         (ns-name (if (and (symbolp name)
                          (find #\. (string name)))
                     ;; For qualified symbols, store as-is for now
                     name
                     name)))
    (setf *current-ns* ns-name)
    ;; Automatically make test helpers available in new namespace
    ;; This is a simplification - real Clojure uses :use to import symbols
    (setup-test-macros env)
    ns-name))

(defun eval-deftest (form env)
  "Evaluate a deftest form: (deftest name body+) - define a test."
  ;; For now, deftest just evaluates the body to check for errors
  ;; It doesn't actually register or run the test yet
  (let* ((name (cadr form))
         (body (cddr form)))
    ;; Evaluate the body forms (they typically use `is` assertions)
    ;; For now, just return the test name
    ;; TODO: Actually collect and run tests
    (dolist (expr body)
      (clojure-eval expr env))
    name))

;;; ============================================================
;;; Closure (function) representation
;;; ============================================================

(defstruct closure
  "A Clojure function closure - captures params, body, and environment."
  params
  body
  env
  (name nil)
  (macro-p nil))  ; true if this is a macro

;;; ============================================================
;;; Core Functions (built-ins)
;;; ============================================================

(defun register-core-function (env name fn)
  "Register a core function in the environment."
  (env-set-var env name fn))

(defun setup-core-functions (env)
  "Set up all core functions in the environment."

  ;; Boolean values
  (env-set-var env 'true t)
  (env-set-var env 'false 'false)

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
  (register-core-function env 'second #'clojure-second)
  (register-core-function env 'rest #'clojure-rest)
  (register-core-function env 'count #'clojure-count)
  (register-core-function env 'vec #'clojure-vec)
  (register-core-function env 'vector #'clojure-vector)
  (register-core-function env 'list #'clojure-list)
  (register-core-function env 'map #'clojure-map)
  (register-core-function env 'apply #'clojure-apply)
  (register-core-function env 'str #'clojure-str)

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
  (register-core-function env 'reduce #'clojure-reduce)

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
      (cond
        ;; For lists, add to front
        ((listp coll)
         (append (reverse xs) coll))
        ;; For vectors, add to end
        ((vectorp coll)
         (coerce (append (coerce coll 'list) xs) 'vector))
        ;; For strings, concatenate
        ((stringp coll)
         (concatenate 'string coll (apply #'concatenate 'string (mapcar #'string xs))))
        ;; For hash tables (sets), just return coll (conj to set not implemented)
        ((hash-table-p coll)
         coll)
        ;; For sequences, convert to list and append
        ((typep coll 'sequence)
         (append (coerce coll 'list) xs))
        ;; For anything else, return as-is
        (t coll))))

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

(defun clojure-second (seq)
  "Return second element of sequence."
  (cond
    ((null seq) nil)
    ((vectorp seq)
     (if (> (length seq) 1)
         (aref seq 1)
         nil))
    ((and (consp seq) (null (cdr seq))) nil)
    ((consp seq) (cadr seq))
    (t nil)))

(defun clojure-str (&rest args)
  "Convert arguments to string and concatenate. With no args, returns empty string."
  (if (null args)
      ""
      (apply #'concatenate 'string (mapcar (lambda (x)
                                              (typecase x
                                                (null "")
                                                (string x)
                                                (character (string x))
                                                (symbol (symbol-name x))
                                                (keyword (symbol-name x))
                                                (t (princ-to-string x))))
                                            args))))

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

(defun clojure-reduce (f init &optional coll)
  "Reduce a collection with a function."
  (if coll
      ;; 3-argument form: (reduce f init coll)
      (if (null coll)
          init
          (reduce f (cdr coll) :initial-value (funcall f init (car coll))))
      ;; 2-argument form: (reduce f coll)
      (if (null init)
          (error "Cannot reduce empty collection")
          (reduce f (cdr init) :initial-value (car init)))))

;;; Predicate implementations
(defun clojure-nil? (x) (null x))
(defun clojure-symbol? (x) (symbolp x))
(defun clojure-keyword? (x) (keywordp x))
(defun clojure-number? (x) (numberp x))
(defun clojure-fn? (x) (closure-p x))
(defun clojure-vector? (x) (vectorp x))

;;; ============================================================
;;; Test Helper Special Forms (from clojure.test)
;;; ============================================================

(defun eval-is (form env)
  "Evaluate an is form: (is expr) - test assertion."
  ;; For now, just evaluate the expression and return it
  ;; TODO: Actually track test results
  (let ((expr (cadr form)))
    (clojure-eval expr env)))

(defun eval-testing (form env)
  "Evaluate a testing form: (testing name & body) - context for tests."
  ;; Evaluate all body forms and return the last result
  (let ((body (cddr form)))
    (if (null body)
        nil
        (let ((last-expr (car (last body))))
          (dolist (expr (butlast body))
            (clojure-eval expr env))
          (clojure-eval last-expr env)))))

(defun eval-are (form env)
  "Evaluate an are form: (are [args] expr & arg-pairs) - multiple assertions."
  ;; For now, just return nil
  ;; TODO: Implement full are semantics
  (declare (ignore env))
  nil)

(defun setup-test-macros (env)
  "Set up test helper symbols - no longer needed as they are special forms."
  (declare (ignore env))
  ;; This is now a no-op since is, testing, are are special forms
  nil)

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
             (rest-form (cdr form))
             (head-name (string-downcase (string (car form)))))
         (cond
           ;; Special forms - compare lowercase symbol names to handle package differences
           ((string= head-name "if") (eval-if form env))
           ((string= head-name "do") (eval-do form env))
           ((string= head-name "quote") (eval-quote form env))
           ((string= head-name "syntax-quote") (eval-syntax-quote form env))
           ((string= head-name "unquote") (eval-unquote form env))
           ((string= head-name "unquote-splicing") (eval-unquote-splicing form env))
           ((string= head-name "var") (eval-var-quote form env))
           ((string= head-name "def") (eval-def form env))
           ((string= head-name "defn") (eval-defn form env))
           ((string= head-name "defmacro") (eval-defmacro form env))
          ((string= head-name "deftest") (eval-deftest form env))
          ((string= head-name "is") (eval-is form env))
          ((string= head-name "testing") (eval-testing form env))
          ((string= head-name "are") (eval-are form env))
           ((string= head-name "fn") (eval-fn form env))
           ((string= head-name "fn*") (eval-fn form env))
           ((string= head-name "let") (eval-let form env))
           ((string= head-name "loop") (eval-loop form env))
          ((string= head-name "ns") (eval-ns form env))

           ;; Function application
           (t
            (let ((fn-value (clojure-eval head env)))
              ;; Check if it's a macro - macros receive unevaluated arguments
              (if (and (closure-p fn-value) (closure-macro-p fn-value))
                  ;; Macro expansion: call with unevaluated args, then eval result
                  (let* ((expanded (apply-function fn-value rest-form)))
                    (clojure-eval expanded env))
                  ;; Normal function: eval args then apply
                  (let ((args (mapcar #'(lambda (x) (clojure-eval x env))
                                      rest-form)))
                    (apply-function fn-value args))))))))

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
         ;; Vector parameters - fixed arity or with & rest params
         ((vectorp params)
          (let* ((amp-pos (position (intern "&") params))
                 (fixed-count (if amp-pos amp-pos (length params))))
            ;; Bind fixed params
            (loop for i from 0 below fixed-count
                  for arg in args
                  do (setf new-env (env-extend-lexical new-env (aref params i) arg)))
            ;; Handle rest param if present
            (when amp-pos
              (let ((rest-param (aref params (1+ amp-pos)))
                    (rest-args (nthcdr fixed-count args)))
                (setf new-env (env-extend-lexical new-env rest-param rest-args))))))
         ;; List parameters (less common but supported)
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
  (setup-test-macros *current-env*)
  *current-env*)

(defun eval-string (string)
  "Evaluate a Clojure string and return the result."
  (unless *current-env*
    (init-eval-system))
  (let* ((form (cl-clojure-syntax:read-clojure-string string)))
    (clojure-eval form *current-env*)))

(defun eval-file (path)
  "Evaluate all forms in a Clojure file."
  (unless *current-env*
    (init-eval-system))
  (let* ((content (with-open-file (s path :direction :input)
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
