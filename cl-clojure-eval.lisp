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
    ;; Initialize with nil, true, and false as special values (use string keys for consistency)
    (setf (gethash (cons "user" "nil") (env-vars env)) (make-var :name 'nil :value nil))
    (setf (gethash (cons "user" "true") (env-vars env)) (make-var :name 'true :value t))
    (setf (gethash (cons "user" "false") (env-vars env)) (make-var :name 'false :value 'false))
    env))

(defun env-get-var (env name &optional (ns '*current-ns*))
  "Get a var from the environment. Returns NIL if not found."
  (let* ((ns-name (if (eq ns '*current-ns*) *current-ns* ns))
         ;; Normalize symbols to lowercase strings for consistent hash keys
         ;; because Clojure's :preserve case mode creates different symbols
         (key (cons (string-downcase (symbol-name ns-name))
                    (string-downcase (symbol-name name)))))
    (or (gethash key (env-vars env))
        (when (env-parent env)
          (env-get-var (env-parent env) name ns)))))

(defun env-intern-var (env name &optional (ns '*current-ns*))
  "Intern a new var in the environment or return existing one."
  (let* ((ns-name (if (eq ns '*current-ns*) *current-ns* ns))
         ;; Normalize symbols to lowercase strings for consistent hash keys
         (key (cons (string-downcase (symbol-name ns-name))
                    (string-downcase (symbol-name name))))
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

(defun eval-ns (form env)
  "Evaluate an ns form: (ns name & args) - namespace declaration.
   For now, this is a no-op that just records the namespace name."
  (declare (ignore env))
  ;; Extract namespace name
  (let* ((name (cadr form))
         ;; Record the current namespace
         (*current-ns* name))
    ;; For now, just return the namespace name
    ;; TODO: Handle :use, :require, :import directives
    name))

(defun eval-deftest (form env)
  "Evaluate a deftest form: (deftest name body+) - define a test.
   The name may be wrapped in a with-meta form, so evaluate it first."
  (let* ((name-expr (cadr form))
         ;; Evaluate the name to handle with-meta
         (name (if (and (consp name-expr) (symbolp (car name-expr)))
                   (clojure-eval name-expr env)
                   name-expr))
         (body (cddr form)))
    ;; Create a closure for the test
    (let ((test-fn (make-closure :params nil
                                 :body body
                                 :env env
                                 :name name)))
      ;; Store the test in the environment
      (env-set-var env name test-fn)
      name)))

(defun eval-declare (form env)
  "Evaluate a declare form: (declare name+) - declare vars without defining them.
   Also handles metadata hints like ^:dynamic."
  (dolist (item (cdr form))
    ;; Skip metadata hints (they start with ^)
    (when (symbolp item)
      ;; Create a var with nil value
      (env-intern-var env item)))
  nil)

(defun eval-set-bang (form env)
  "Evaluate a set! form: (set! var expr) - set the value of a var."
  ;; set! can be used in two forms:
  ;; (set! var-name value) - set a var's value
  ;; (set! (.instance-field obj) value) - set a Java field (not implemented yet)
  (let* ((target (cadr form))
         (value-expr (caddr form))
         (value (clojure-eval value-expr env)))
    ;; For now, just handle simple symbol targets (vars)
    ;; TODO: Handle Java field assignment
    (cond
      ;; Symbol target - set the var's value
      ((symbolp target)
       (let ((var (env-get-var env target)))
         (if var
             (progn
               (setf (var-value var) value)
               value)
             ;; If var doesn't exist, create it and set
             (env-set-var env target value))))
      ;; TODO: Handle Java interop forms like (.field obj)
      (t (error "Unsupported set! target: ~A" target)))))

(defun eval-with-meta (form env)
  "Evaluate a with-meta form: (with-meta obj metadata) - attach metadata to an object.
   For symbols (like in deftest names), we just return the symbol itself
   since the metadata is attached to the var, not the value.
   For other objects, we would need a way to store metadata separately."
  (declare (ignore env))
  ;; In Clojure, metadata is attached to objects (especially symbols and vars)
  ;; For eval purposes in deftest, we can ignore the metadata and just return
  ;; the underlying form (usually a symbol)
  (let ((obj (cadr form))
        (metadata (caddr form)))
    (declare (ignore metadata))
    ;; For now, just return the object without metadata
    ;; A full implementation would store metadata somewhere
    obj))

(defun eval-case (form env)
  "Evaluate a case form: (case expr test1 result1 test2 result2 ... default?)
   Compares expr (using =) against each test and returns the corresponding result.
   If no match and no default, returns nil."
  (let* ((expr (cadr form))
         (expr-value (clojure-eval expr env))
         (clauses (cddr form)))
    ;; Process clauses pairwise
    (loop for (test result) on clauses by #'cddr
          while test
          do (cond
               ;; Last clause with no result pair - this is the default
               ((null (cdr (member test clauses)))
                ;; This is the default case
                (return-from eval-case (clojure-eval test env)))
               ;; Special case: test can be a list of values (v1 v2 v3)
               ((and (listp test) (not (null test)))
                ;; Check if expr-value matches any of the test values
                (if (find expr-value test :test #'clojure=)
                    (return-from eval-case (clojure-eval result env))
                    ;; Continue to next clause
                    nil))
               ;; Single value test
               (t
                (if (clojure= expr-value test)
                    (return-from eval-case (clojure-eval result env))
                    ;; Continue to next clause
                    nil))))
    ;; No match found and no default clause
    nil))

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
;;; Atom (mutable reference) representation
;;; ============================================================

(defstruct clojure-atom
  "A Clojure atom - a mutable reference."
  value)

;;; ============================================================
;;; Test Helpers (no-ops for now)
;;; ============================================================

(defun clojure-deftest (name &rest body)
  "Define a test. Intern the test name as a var."
  ;; Store the test function in the environment
  (when *current-env*
    (env-set-var *current-env* name
                  (make-closure :params nil
                                :body body
                                :env *current-env*
                                :name name)))
  name)

(defun clojure-testing (name &rest body)
  "Define a test context. For now, just evaluate body and return nil."
  (declare (ignore name))
  ;; Evaluate all body forms
  (when *current-env*
    (dolist (form body)
      (clojure-eval form *current-env*)))
  nil)

(defun clojure-is (condition &rest msg)
  "Assert a condition. For now, just return the condition."
  (declare (ignore msg))
  condition)

(defun clojure-use-fixtures (fixtures &rest tests)
  "Apply fixtures to tests. For now, no-op."
  (declare (ignore fixtures tests))
  nil)

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

  ;; Test helpers
  (register-core-function env 'deftest #'clojure-deftest)
  (register-core-function env 'testing #'clojure-testing)
  (register-core-function env 'is #'clojure-is)
  (register-core-function env 'use-fixtures #'clojure-use-fixtures)

  ;; Atom functions
  (register-core-function env 'atom #'clojure-atom)
  (register-core-function env 'swap! #'clojure-swap!)
  (register-core-function env 'reset! #'clojure-reset!)
  (register-core-function env 'deref #'clojure-deref)

  ;; File loading
  (register-core-function env 'load #'clojure-load)

  ;; Java interop stubs - System/getProperty
  ;; Note: Use lowercase because env-get-var normalizes to lowercase
  (setf (gethash (cons "user" "system/getproperty") (env-vars env))
        (make-var :name 'System/getProperty :value #'clojure-get-property))

  env)

;;; Load function
(defvar *load-path* nil
  "Search path for loading Clojure files.")

(defun clojure-load (path &optional relative-to)
  "Load a Clojure file from the given path.
   The path can be a string (without .clj extension) or a full path.
   For now, this is a no-op that returns nil.
   TODO: Implement actual file loading."
  (declare (ignore path relative-to))
  ;; For now, just return nil since we're not loading Java-specific code
  ;; In a full implementation, this would:
  ;; 1. Find the file on the classpath/load-path
  ;; 2. Add .clj extension if needed
  ;; 3. Read and evaluate all forms in the file
  nil)

;;; Java interop stubs
(defun clojure-get-property (&rest args)
  "Stub for Java System/getProperty.
   Returns nil so case tests fall through to the default.
   The real call would be (Class/method args...) but for now we just return nil."
  (declare (ignore args))
  nil)

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
  (cond
    ((null xs) coll)
    ((null coll) (apply #'clojure-list xs))
    ((listp coll) (append (reverse xs) coll))
    ((vectorp coll)
     ;; Vector - add elements to end
     (coerce (concatenate 'list coll xs) 'vector))
    (t coll)))

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

;;; Atom implementations
(defun clojure-atom (initial-value)
  "Create a new atom with initial-value."
  (make-clojure-atom :value initial-value))

(defun clojure-deref (atom-ref)
  "Dereference an atom (or ref). Returns the current value."
  (if (clojure-atom-p atom-ref)
      (clojure-atom-value atom-ref)
      ;; For other ref types, return as-is for now
      atom-ref))

(defun clojure-reset! (atom-ref new-value)
  "Reset an atom to a new value. Returns the new value."
  (when (clojure-atom-p atom-ref)
    (setf (clojure-atom-value atom-ref) new-value))
  new-value)

(defun clojure-swap! (atom-ref fn-arg &rest args)
  "Atomically swap the value of an atom by applying fn to current value and args."
  (when (clojure-atom-p atom-ref)
    (let ((new-value (apply fn-arg (clojure-atom-value atom-ref) args)))
      (setf (clojure-atom-value atom-ref) new-value)
      new-value)))

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
           ;; Special forms - compare by symbol name (lowercase for Clojure compatibility)
           ((and (symbolp head) (string-equal (symbol-name head) "if")) (eval-if form env))
           ((and (symbolp head) (string-equal (symbol-name head) "do")) (eval-do form env))
           ((and (symbolp head) (string-equal (symbol-name head) "quote")) (eval-quote form env))
           ((and (symbolp head) (string-equal (symbol-name head) "var")) (eval-var-quote form env))
           ((and (symbolp head) (string-equal (symbol-name head) "def")) (eval-def form env))
           ((and (symbolp head) (string-equal (symbol-name head) "defn")) (eval-defn form env))
           ((and (symbolp head) (string-equal (symbol-name head) "fn")) (eval-fn form env))
           ((and (symbolp head) (string-equal (symbol-name head) "fn*")) (eval-fn form env))
           ((and (symbolp head) (string-equal (symbol-name head) "let")) (eval-let form env))
           ((and (symbolp head) (string-equal (symbol-name head) "let*")) (eval-let form env))
           ((and (symbolp head) (string-equal (symbol-name head) "loop")) (eval-loop form env))
           ((and (symbolp head) (string-equal (symbol-name head) "ns")) (eval-ns form env))
           ((and (symbolp head) (string-equal (symbol-name head) "deftest")) (eval-deftest form env))
           ((and (symbolp head) (string-equal (symbol-name head) "declare")) (eval-declare form env))
           ((and (symbolp head) (string-equal (symbol-name head) "set!")) (eval-set-bang form env))
           ((and (symbolp head) (string-equal (symbol-name head) "with-meta")) (eval-with-meta form env))
           ((and (symbolp head) (string-equal (symbol-name head) "case")) (eval-case form env))

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
         ((vectorp params)
          (loop for param across params
                for arg in args
                do (setf new-env (env-extend-lexical new-env param arg))))
         ;; Vector parameters with & rest params
         ((and (vectorp params)
               (> (length params) 1)
               (find '& params))
          (let* ((&-pos (position '& params))
                 (rest-params (subseq params (1+ &-pos)))
                 (fixed-count &-pos)
                 (rest-args (nthcdr fixed-count args)))
            ;; Bind fixed params
            (loop for i from 0 below &-pos
                  for arg in args
                  do (setf new-env (env-extend-lexical new-env (aref params i) arg)))
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
  ;; Use dynamic binding for readtable so we don't affect global state
  (let* ((*readtable* (cl-clojure-syntax:ensure-clojure-readtable))
         (form (cl-clojure-syntax:read-clojure-string string)))
    (clojure-eval form *current-env*)))

(defun eval-file (path)
  "Evaluate all forms in a Clojure file."
  (unless *current-env*
    (init-eval-system))
  (let* ((raw-content (with-open-file (s path :direction :input)
                        (let ((str (make-string (file-length s))))
                          (read-sequence str s)
                          str)))
         (preprocessed (cl-clojure-syntax:preprocess-clojure-dots raw-content))
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
