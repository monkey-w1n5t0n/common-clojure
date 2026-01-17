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

(defun eval-and (form env)
  "Evaluate an and form: (and expr*) - returns nil if any expr is falsey, else last value."
  (let ((forms (cdr form)))
    (if (null forms)
        ;; No arguments - return true
        'true
        ;; Evaluate forms until one is falsey
        (let ((result 'true))
          (dolist (expr forms)
            (setf result (clojure-eval expr env))
            (when (falsey? result)
              (return-from eval-and nil)))
          result))))

(defun eval-or (form env)
  "Evaluate an or form: (or expr*) - returns first truthy value, or nil if all falsey."
  (let ((forms (cdr form)))
    (if (null forms)
        ;; No arguments - return nil
        nil
        ;; Evaluate forms until one is truthy
        (block nil
          (dolist (expr forms)
            (let ((result (clojure-eval expr env)))
              (when (truthy? result)
                (return-from nil result))))))))

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
  ;; Handle metadata on the name: (def ^:dynamic name expr)
  (let* ((second (cadr form))
         (has-metadata (and (consp second)
                           (string= (symbol-name (car second)) "WITH-META")))
         (name (if has-metadata (cadr second) second))
         (value-expr (if has-metadata
                        (caddr form)
                        (caddr form)))
         (value (if value-expr
                   (clojure-eval value-expr env)
                   nil)))
    (env-set-var env name value)
    name))

(defun eval-declare (form env)
  "Evaluate a declare form: (declare name+) - forward declare vars."
  ;; declare creates vars without values (for forward declarations)
  ;; Names can have metadata: (declare ^:dynamic name)
  (flet ((extract-name (spec)
           "Extract the name from a spec, which might be (with-meta name metadata) or just a name."
           (if (and (consp spec) (string= (symbol-name (car spec)) "WITH-META"))
               (cadr spec)  ; (with-meta name metadata) -> name
               spec)))       ; just name
    (let* ((names (cdr form)))
      (dolist (name-spec names)
        (let ((name (extract-name name-spec)))
          (env-intern-var env name)))
      nil)))

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

(defun eval-for (form env)
  "Evaluate a for form: (for [seq-exprs body-expr) - list comprehension.
   seq-exprs are (binding expr) optionally followed by :let, :when, :while modifiers.
   Returns a lazy sequence of body-expr results."
  (let* ((bindings-body (cdr form))
         (bindings (car bindings-body))
         (body-expr (cadr bindings-body)))
    ;; Convert vector bindings to list
    (let ((binding-list (if (vectorp bindings)
                            (coerce bindings 'list)
                            bindings)))
      ;; Parse clauses into binding specs with local modifiers
      ;; and evaluate the for comprehension
      (eval-for-nested (parse-for-clauses binding-list) body-expr env))))

(defun parse-for-clauses (clauses)
  "Parse for clauses into ((binding-expr . local-modifiers)...) structure.
   Each binding-expr is (symbol . collection-expr) and local-modifiers is a list
   of modifiers that apply to this specific binding.
   E.g., [x (range 10) :when (odd? x) y (range 4)]
   => (((x . (range 10)) ((:when . (odd? x)))) ((y . (range 4)) ()))"
  (let (result)
    (loop while clauses
          do (let ((clause (car clauses))
                   (rest (cdr clauses)))
               (cond
                 ;; Keyword modifiers - collect for current binding
                 ((and (symbolp clause)
                       (keywordp clause)
                       (member (symbol-name clause) '("when" "while" "let") :test #'string=))
                  (when (null rest)
                    (error "Missing value for ~A in for" clause))
                  (when (null result)
                    (error "Modifier ~A without preceding binding" clause))
                  ;; Add modifier to the most recent binding's modifiers
                  (setf (cdr (car result))
                        (cons (list clause (car rest)) (cdr (car result))))
                  (setf clauses (cdr rest)))
                 ;; Regular binding: symbol expr
                 ((symbolp clause)
                  (when (null rest)
                    (error "Missing expression for binding ~A in for" clause))
                  (push (cons (cons clause (car rest)) nil) result)
                  (setf clauses (cdr rest)))
                 (t
                  (error "Invalid clause in for: ~A" clause)))))
    (values (mapcar (lambda (binding)
                      (cons (car binding)
                            (nreverse (cdr binding))))
                    (nreverse result)))))

(defun apply-local-modifiers (env modifiers)
  "Apply local modifiers (:when, :while, :let) to an environment.
   Returns NIL if :when fails or :while fails (filter out this iteration).
   Returns updated env if :let is present or all modifiers pass."
  (dolist (modifier modifiers env)
    (let ((key (car modifier))
          (value-expr (cdr modifier)))
      (cond
        ((eq key :when)
         (let ((test-val (clojure-eval value-expr env)))
           (when (falsey? test-val)
             (return-from apply-local-modifiers nil))))
        ((eq key :while)
         (let ((test-val (clojure-eval value-expr env)))
           (when (falsey? test-val)
             (return-from apply-local-modifiers nil))))
        ((eq key :let)
         ;; :let creates new bindings - value-expr should be a vector of bindings
         ;; For now, we only handle simple :let [binding expr] forms
         (when (vectorp value-expr)
           (let ((bindings-list (coerce value-expr 'list)))
             (loop for (name val-expr) on bindings-list by #'cddr
                   while name
                   do (let ((val (clojure-eval val-expr env)))
                        (setf env (env-extend-lexical env name val)))))))))))

(defun eval-for-nested (bindings body-expr env)
  "Evaluate nested for comprehension, producing list of results.
   bindings is (((symbol . unevaluated-expr) . local-modifiers)...) - exprs are
   evaluated with current env, and modifiers apply to each iteration of that binding."
  (if (null bindings)
      ;; Base case: evaluate body
      ;; Vectors need to be evaluated element-wise
      (let ((result (if (vectorp body-expr)
                       (coerce (mapcar (lambda (x) (clojure-eval x env))
                                      (coerce body-expr 'list))
                               'vector)
                       (clojure-eval body-expr env))))
        (list result))
      ;; Recursive case: iterate over first collection
      (let* ((first-binding-spec (car bindings))
             (first-binding (caar first-binding-spec))
             (first-expr (cdar first-binding-spec))
             (local-modifiers (cdr first-binding-spec))
             (rest-bindings (cdr bindings)))
        ;; Evaluate the collection expression with current env
        (let* ((first-coll (clojure-eval first-expr env))
               (results nil))
          ;; Handle lazy ranges specially to avoid heap exhaustion
          (cond
            ((lazy-range-p first-coll)
             (let ((start (lazy-range-start first-coll))
                   (end (lazy-range-end first-coll))
                   (step (lazy-range-step first-coll)))
               (if end
                   ;; Bounded range - iterate but limit to avoid heap issues
                   (let ((iter-limit (min 10000 (- end start))))
                     (loop for i from start below end by step
                           for iter-count from 0
                           while (< iter-count iter-limit)
                           do (let* ((new-env (env-extend-lexical env first-binding i))
                                     (filtered-env (apply-local-modifiers new-env local-modifiers)))
                                (when filtered-env
                                  (let ((nested-results (eval-for-nested rest-bindings
                                                                        body-expr
                                                                        filtered-env)))
                                    (setf results (append results nested-results)))))))
                   ;; Infinite range - limit iterations
                   (loop for i from start by step
                         repeat 1000
                         do (let* ((new-env (env-extend-lexical env first-binding i))
                                   (filtered-env (apply-local-modifiers new-env local-modifiers)))
                              (when filtered-env
                                (let ((nested-results (eval-for-nested rest-bindings
                                                                          body-expr
                                                                          filtered-env)))
                                  (setf results (append results nested-results)))))))))
            ;; Regular collection - convert to list
            (t
             (let ((first-coll-list (if (listp first-coll)
                                        first-coll
                                        (coerce first-coll 'list))))
               (dolist (elem first-coll-list)
                 (let* ((new-env (env-extend-lexical env first-binding elem))
                        (filtered-env (apply-local-modifiers new-env local-modifiers)))
                   (when filtered-env
                     (let ((nested-results (eval-for-nested rest-bindings
                                                               body-expr
                                                               filtered-env)))
                       (setf results (append results nested-results)))))))))
          results))))

(defun eval-doseq (form env)
  "Evaluate a doseq form: (doseq [seq-exprs] body+) - execute body for side effects.
   Like for but returns nil. seq-exprs are (binding expr) optionally followed
   by :let, :when, :while modifiers."
  (let* ((bindings-body (cdr form))
         (bindings (car bindings-body))
         (body-exprs (cadr bindings-body)))
    ;; Convert vector bindings to list
    (let ((binding-list (if (vectorp bindings)
                            (coerce bindings 'list)
                            bindings)))
      ;; Execute the doseq comprehension (returns nil)
      (eval-doseq-nested (parse-for-clauses binding-list) body-exprs env)
      nil)))

(defun eval-doseq-nested (bindings body-exprs env)
  "Evaluate nested doseq comprehension, executing body-exprs for each iteration.
   Returns nil (doseq is for side effects only)."
  (if (null bindings)
      ;; Base case: evaluate body expressions
      (dolist (expr (if (listp body-exprs) body-exprs (list body-exprs)))
        (clojure-eval expr env))
      ;; Recursive case: iterate over first collection
      (let* ((first-binding-spec (car bindings))
             (first-binding (caar first-binding-spec))
             (first-expr (cdar first-binding-spec))
             (local-modifiers (cdr first-binding-spec))
             (rest-bindings (cdr bindings)))
        ;; Evaluate the collection expression with current env
        (let* ((first-coll (clojure-eval first-expr env)))
          ;; Handle lazy ranges specially
          (cond
            ((lazy-range-p first-coll)
             (let ((start (lazy-range-start first-coll))
                   (end (lazy-range-end first-coll))
                   (step (lazy-range-step first-coll)))
               (if end
                   ;; Bounded range
                   (loop for i from start below end by step
                         do (let* ((new-env (env-extend-lexical env first-binding i))
                                   (filtered-env (apply-local-modifiers new-env local-modifiers)))
                              (when filtered-env
                                (eval-doseq-nested rest-bindings body-exprs filtered-env))))
                   ;; Infinite range - limit iterations
                   (loop for i from start by step
                         repeat 1000
                         do (let* ((new-env (env-extend-lexical env first-binding i))
                                   (filtered-env (apply-local-modifiers new-env local-modifiers)))
                              (when filtered-env
                                (eval-doseq-nested rest-bindings body-exprs filtered-env)))))))
            ;; Regular collection
            (t
             (let ((first-coll-list (if (listp first-coll)
                                        first-coll
                                        (coerce first-coll 'list))))
               (dolist (elem first-coll-list)
                 (let* ((new-env (env-extend-lexical env first-binding elem))
                        (filtered-env (apply-local-modifiers new-env local-modifiers)))
                   (when filtered-env
                     (eval-doseq-nested rest-bindings body-exprs filtered-env)))))))))))

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

(defun eval-import (form env)
  "Evaluate an import form: (import & import-lists) - import Java classes.
   Since we're on SBCL without Java interop, this is a no-op that just records imports.
   Supports:
   - (import 'java.lang.String)
   - (import '(java.util List Map))
   - (import [java.util List Map])"
  (declare (ignore env))
  ;; For SBCL without Java, we just return nil - the import syntax is parsed but does nothing
  ;; Real Clojure would add these to the namespace's imports map
  nil)

(defun eval-set-bang (form env)
  "Evaluate a set! form: (set! var-name value) - set the value of a var.
   For now, this is a simplified version that just stores the value."
  ;; (set! target value) - sets the value of target to value
  ;; Target can be a var, a field (Java interop), or an atomic reference
  (let* ((target (cadr form))
         (value-expr (caddr form))
         (value (clojure-eval value-expr env)))
    ;; For now, just set the var's value if it exists
    ;; Real Clojure would handle Java fields, atoms, refs, etc.
    (typecase target
      ;; If target is a symbol, set the var's value
      (symbol
       (let ((var (env-get-var env target)))
         (if var
             (progn
               (setf (var-value var) value)
               value)
             ;; If no var exists, create one
             (env-set-var env target value))))
      ;; Otherwise return the value (no-op for unsupported targets)
      (t value))))

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
;;; Lazy Range representation
;;; ============================================================

(defstruct lazy-range
  "A lazy range - generates numbers on demand to avoid heap exhaustion."
  start
  end
  step
  (current 0))

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
  (register-core-function env 'into #'clojure-into)
  (register-core-function env 'concat #'clojure-concat)
  (register-core-function env 'range #'clojure-range)
  (register-core-function env 'into-array #'clojure-into-array)

  ;; Predicate functions
  (register-core-function env 'nil? #'clojure-nil?)
  (register-core-function env 'symbol? #'clojure-symbol?)
  (register-core-function env 'keyword? #'clojure-keyword?)
  (register-core-function env 'number? #'clojure-number?)
  (register-core-function env 'fn? #'clojure-fn?)
  (register-core-function env 'vector? #'clojure-vector?)
  (register-core-function env 'not #'clojure-not)
  (register-core-function env 'some? #'clojure-some?)

  ;; Sequence functions
  (register-core-function env 'seq #'clojure-seq)
  (register-core-function env 'identity #'clojure-identity)
  (register-core-function env 'reduce #'clojure-reduce)
  (register-core-function env 'eval #'clojure-eval-fn)
  (register-core-function env 'take #'clojure-take)
  (register-core-function env 'every? #'clojure-every?)
  (register-core-function env 'some #'clojure-some)
  (register-core-function env 'not-every? #'clojure-not-every?)
  (register-core-function env 'not-any? #'clojure-not-any?)

  ;; String/Symbol functions
  (register-core-function env 'symbol #'clojure-symbol)
  (register-core-function env 'atom #'clojure-atom)
  (register-core-function env 'read-string #'clojure-read-string)
  (register-core-function env 'println #'clojure-println)
  (register-core-function env 'prn #'clojure-prn)

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
      (let* ((processed-args (mapcar (lambda (x)
                                       (if (lazy-range-p x)
                                           (lazy-range-to-list x)
                                           x))
                                     args)))
        (apply #'equal processed-args))))

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
  (cond
    ((null seq) nil)
    ((lazy-range-p seq)
     (let ((start (lazy-range-start seq))
           (end (lazy-range-end seq)))
       (if (and end (>= start end))
           nil
           start)))
    ((and (consp seq) (null (car seq))) nil)
    (t (car seq))))

(defun clojure-rest (seq)
  "Return rest of sequence."
  (cond
    ((null seq) '())
    ((lazy-range-p seq)
     (let ((start (lazy-range-start seq))
           (end (lazy-range-end seq))
           (step (lazy-range-step seq)))
       (let ((new-start (+ start step)))
         (if (and end (>= new-start end))
             '()
             (make-lazy-range :start new-start :end end :step step :current new-start)))))
    ((and (consp seq) (null (cdr seq))) '())
    (t (cdr seq))))

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
    ((lazy-range-p coll)
     (if (lazy-range-end coll)
         (let ((start (lazy-range-start coll))
               (end (lazy-range-end coll))
               (step (lazy-range-step coll)))
           (ceiling (/ (- end start) step)))
         ;; For infinite ranges, return a special marker
         1000000))
    ((listp coll) (length coll))
    ((vectorp coll) (length coll))
    ((stringp coll) (length coll))
    (t 0)))

(defun clojure-vec (coll)
  "Create a vector from collection."
  (cond
    ((vectorp coll) coll)
    ((lazy-range-p coll) (coerce (lazy-range-to-list coll) 'vector))
    (t (coerce coll 'vector))))

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
    (cond
      ((lazy-range-p coll) coll)
      ((listp coll) coll)
      (t (coerce coll 'list)))))

(defun clojure-into (to from)
  "Conjoin elements from `from` into `to`.
   (into [] coll) returns a vector with coll's elements
   (into () coll) returns a list with coll's elements
   (into x y) uses conj to add y's elements to x"
  (let ((from-seq (cond
                    ((lazy-range-p from) (lazy-range-to-list from))
                    ((listp from) from)
                    (t (coerce from 'list)))))
    (cond
      ;; If to is a vector, build a new vector
      ((vectorp to)
       (coerce (append (coerce to 'list) from-seq) 'vector))
      ;; If to is a list, append to front (reverse, append, reverse)
      ((listp to)
       (append (reverse from-seq) to))
      ;; If to is nil or empty, return from as-is
      ((null to)
       from-seq)
      ;; Default: just return from (not fully implemented)
      (t from))))

(defun clojure-concat (&rest colls)
  "Concatenate sequences together. Returns a lazy sequence."
  ;; Flatten all collections into a single list
  (let ((result '()))
    (dolist (coll (reverse colls))
      (when coll
        (let ((coll-list (cond
                           ((lazy-range-p coll) (lazy-range-to-list coll))
                           ((listp coll) coll)
                           (t (coerce coll 'list)))))
          (setf result (append coll-list result)))))
    result))

(defun clojure-range (&optional (start 0) end step)
  "Return a lazy sequence of numbers from start (inclusive) to end (exclusive).
   (range) -> infinite range starting from 0
   (range 10) -> 0 1 2 ... 9
   (range 1 10) -> 1 2 3 ... 9
   (range 1 10 2) -> 1 3 5 7 9"
  (cond
    ;; No arguments - infinite range starting from 0
    ((and (null end) (null step))
     (make-lazy-range :start 0 :end nil :step 1 :current 0))
    ;; One argument: treat as end, start from 0
    ((null end)
     (if (null start)
         '()
         (make-lazy-range :start 0 :end start :step 1 :current 0)))
    ;; Two or three arguments
    (t
     (let ((actual-step (if (null step) 1 step)))
       (make-lazy-range :start start :end end :step actual-step :current start)))))

(defun lazy-range-to-list (lr &optional (limit most-positive-fixnum))
  "Convert a lazy range to a list, up to limit elements."
  (if (not (lazy-range-p lr))
      lr
      (let ((start (lazy-range-start lr))
            (end (lazy-range-end lr))
            (step (lazy-range-step lr)))
        (if end
            ;; Bounded range
            (loop for i from start below end by step
                  while (> limit 0)
                  collect i
                  do (decf limit))
            ;; Infinite range - use limit
            (loop for i from start by step
                  repeat limit
                  collect i)))))

(defun realize-range (lr)
  "Realize a lazy range to a concrete list. For infinite ranges, limit to 1000."
  (if (lazy-range-p lr)
      (lazy-range-to-list lr (if (lazy-range-end lr) most-positive-fixnum 1000))
      lr))

(defun clojure-into-array (aseq &optional type)
  "Convert a sequence to a Java array. For SBCL, we return a vector instead."
  (declare (ignore type))
  (let ((seq-to-convert (cond
                          ((lazy-range-p aseq) (lazy-range-to-list aseq))
                          ((listp aseq) aseq)
                          (t (coerce aseq 'list)))))
    (coerce seq-to-convert 'vector)))

(defun clojure-identity (x) x)

(defun clojure-reduce (f init &optional coll)
  "Reduce a collection with a function."
  (if coll
      ;; 3-argument form: (reduce f init coll)
      (if (null coll)
          init
          (let ((coll-list (if (lazy-range-p coll)
                               (lazy-range-to-list coll)
                               coll)))
            (reduce f (cdr coll-list) :initial-value (funcall f init (car coll-list)))))
      ;; 2-argument form: (reduce f coll)
      (if (null init)
          (error "Cannot reduce empty collection")
          (let ((coll-list (if (lazy-range-p init)
                               (lazy-range-to-list init)
                               init)))
            (reduce f (cdr coll-list) :initial-value (car coll-list))))))

(defun clojure-eval-fn (form)
  "Evaluate a Clojure form at runtime. This is the eval function available to Clojure code."
  (clojure-eval form *current-env*))

(defun clojure-take (n coll)
  "Return the first n elements of a collection."
  (if (or (null coll) (<= n 0))
      '()
      (cond
        ((lazy-range-p coll)
         ;; Return a new lazy range with the end adjusted
         (let ((start (lazy-range-start coll))
               (end (lazy-range-end coll))
               (step (lazy-range-step coll)))
           (if end
               (make-lazy-range :start start
                               :end (min end (+ start (* n step)))
                               :step step
                               :current start)
               (make-lazy-range :start start
                               :end (+ start (* n step))
                               :step step
                               :current start))))
        ((listp coll)
         (loop for i from 1 to n
               for elem in coll
               collect elem
               while (< i (length coll))))
        (t (let ((coll-list (coerce coll 'list)))
             (loop for i from 1 to n
                   for elem in coll-list
                   collect elem
                   while (< i (length coll-list))))))))

(defun clojure-every? (pred coll)
  "Return true if pred is true for every element in coll."
  (cond
    ((null coll) 'true)
    ((lazy-range-p coll)
     (let ((start (lazy-range-start coll))
           (end (lazy-range-end coll))
           (step (lazy-range-step coll)))
       (if end
           (loop for i from start below end by step
                 always (funcall pred i))
           ;; For infinite ranges, limit to 1000 elements
           (loop for i from start by step
                 repeat 1000
                 always (funcall pred i)))))
    ((listp coll)
     (every pred coll))
    (t
     (every pred (coerce coll 'list)))))

(defun clojure-some (pred coll)
  "Return the first truthy value of (pred x) for any x in coll, or nil if none."
  (cond
    ((null coll) nil)
    ((lazy-range-p coll)
     (let ((start (lazy-range-start coll))
           (end (lazy-range-end coll))
           (step (lazy-range-step coll)))
       (if end
           (loop for i from start below end by step
                 thereis (funcall pred i))
           ;; For infinite ranges, limit to 1000 elements
           (loop for i from start by step
                 repeat 1000
                 thereis (funcall pred i)))))
    ((listp coll)
     (some pred coll))
    (t
     (some pred (coerce coll 'list)))))

(defun clojure-not-every? (pred coll)
  "Return true if pred is not true for every element in coll (i.e., false for some element)."
  (if (clojure-every? pred coll)
      nil
      'true))

(defun clojure-not-any? (pred coll)
  "Return true if pred is not true for any element in coll."
  (if (clojure-some pred coll)
      nil
      'true))

;;; Predicate implementations
(defun clojure-nil? (x) (null x))
(defun clojure-symbol? (x) (symbolp x))
(defun clojure-keyword? (x) (keywordp x))
(defun clojure-number? (x) (numberp x))
(defun clojure-fn? (x) (closure-p x))
(defun clojure-vector? (x) (vectorp x))
(defun clojure-not (x)
  "Return true if x is falsey (nil or false)."
  (if (or (null x) (eq x 'false))
      'true
      nil))
(defun clojure-some? (x)
  "Return true if x is not nil."
  (if (null x)
      nil
      'true))

;;; String/Symbol constructors
(defun clojure-symbol (name &optional ns)
  "Create a symbol from a string or string+namespace."
  (if ns
      (let ((ns-str (if (stringp ns) ns (string ns)))
            (name-str (if (stringp name) name (string name))))
        (intern (concatenate 'string ns-str "/" name-str)))
      (if (stringp name)
          (intern name)
          (intern (string name)))))

(defun clojure-atom (value)
  "Create an atom (a mutable reference). Returns a mutable container.
   For now, atoms are just cons cells with the value in the car."
  (let ((atom-container (list value)))
    ;; Store metadata on the first element to mark this as an atom
    ;; We need to store it on a symbol, not the list itself
    (setf (get 'atom-marker t) t)
    atom-container))

(defun clojure-read-string (string &optional (eof-error-p t) (eof-value :eof))
  "Read a single Clojure form from a string.
   Returns the read object. If no form is found, returns eof-value."
  (let ((preprocessed (cl-clojure-syntax:preprocess-clojure-dots string)))
    (with-input-from-string (stream preprocessed)
      (let ((*readtable* (cl-clojure-syntax:ensure-clojure-readtable)))
        (cl-clojure-syntax:read-clojure stream eof-error-p eof-value)))))

(defun clojure-println (&rest args)
  "Print arguments to standard output, followed by a newline.
   Each argument is converted to a string via str."
  (dolist (arg args)
    (princ (clojure-str arg)))
  (terpri)
  nil)

(defun clojure-prn (&rest args)
  "Print arguments to standard output in readable form, followed by a newline."
  (dolist (arg args)
    (prin1 arg))
  (terpri)
  nil)

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
;;; Form Normalization
;;; ============================================================

(defun unwrap-with-meta (form)
  "Unwrap with-meta from form, returning (values normalized-form metadata).
   For example: (with-meta foo {:bar true}) -> (values foo {:bar true})"
  (if (and (consp form)
           (symbolp (car form))
           (string= (string-downcase (symbol-name (car form))) "with-meta"))
      (destructuring-bind (with-meta-sym value metadata) form
        (declare (ignore with-meta-sym))
        (values value metadata))
      (values form nil)))

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
       ;; First, unwrap with-meta from head if present (e.g., ^:once fn*)
       (let* ((raw-head (car form))
              (rest-form (cdr form))
              (head (multiple-value-bind (unwrapped metadata)
                        (unwrap-with-meta raw-head)
                      (declare (ignore metadata))  ; TODO: store metadata
                      unwrapped))
              (head-name (when (symbolp head)
                          (string-downcase (symbol-name head)))))
         (cond
           ;; Special forms - compare lowercase symbol names to handle package differences
           ;; Only check if head is a symbol (head-name is not nil)
           ((and head-name (string= head-name "if")) (eval-if form env))
           ((and head-name (string= head-name "do")) (eval-do form env))
           ((and head-name (string= head-name "and")) (eval-and form env))
           ((and head-name (string= head-name "or")) (eval-or form env))
           ((and head-name (string= head-name "quote")) (eval-quote form env))
           ((and head-name (string= head-name "syntax-quote")) (eval-syntax-quote form env))
           ((and head-name (string= head-name "quasiquote")) (eval-syntax-quote form env))  ; CL's QUASIQUOTE
           ((and head-name (string= head-name "unquote")) (eval-unquote form env))
           ((and head-name (string= head-name "unquote-splicing")) (eval-unquote-splicing form env))
           ((and head-name (string= head-name "var")) (eval-var-quote form env))
           ((and head-name (string= head-name "def")) (eval-def form env))
           ((and head-name (string= head-name "defn")) (eval-defn form env))
           ((and head-name (string= head-name "defmacro")) (eval-defmacro form env))
           ((and head-name (string= head-name "deftest")) (eval-deftest form env))
           ((and head-name (string= head-name "is")) (eval-is form env))
           ((and head-name (string= head-name "testing")) (eval-testing form env))
           ((and head-name (string= head-name "are")) (eval-are form env))
           ((and head-name (string= head-name "fn")) (eval-fn form env))
           ((and head-name (string= head-name "fn*")) (eval-fn form env))
           ((and head-name (string= head-name "let")) (eval-let form env))
           ((and head-name (string= head-name "loop")) (eval-loop form env))
           ((and head-name (string= head-name "for")) (eval-for form env))
           ((and head-name (string= head-name "doseq")) (eval-doseq form env))
           ((and head-name (string= head-name "ns")) (eval-ns form env))
           ((and head-name (string= head-name "import")) (eval-import form env))
           ((and head-name (string= head-name "set!")) (eval-set-bang form env))
           ((and head-name (string= head-name "declare")) (eval-declare form env))
           ((and head-name (string= head-name "with-meta"))
            ;; with-meta attaches metadata to a value
            ;; (with-meta value metadata) -> value with metadata
            ;; For now, we just evaluate the value and ignore metadata
            ;; TODO: Actually store and propagate metadata
            (destructuring-bind (with-meta-sym value metadata) form
              (declare (ignore with-meta-sym metadata))
              (clojure-eval value env)))
           ((and head-name (string= head-name "case"))
            ;; Case form: evaluate expr, then compare against each clause
            (let* ((expr-expr (cadr form))
                   (clauses (cddr form))
                   (expr-value (clojure-eval expr-expr env)))
              (loop for (test result) on clauses by (function cddr)
                    when (null test)
                      do (return-from clojure-eval (clojure-eval result env))
                    when (equal expr-value (clojure-eval test env))
                      do (return-from clojure-eval (clojure-eval result env))
                    finally (return-from clojure-eval nil))))

           ;; Function application (including when head is not a symbol)
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
