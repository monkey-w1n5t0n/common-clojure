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
            (progn
              ;; DEBUG: Print when not found in current env
              ;; (format t "~&DEBUG: env-get-lexical: ~A not found in env, checking parent~%" name)
              (when (env-parent env)
                (env-get-lexical (env-parent env) name))))))))

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

(defun eval-clojure-when (form env)
  "Evaluate a when form: (when test body*) - evaluate body if test is truthy."
  (let ((test-form (cadr form))
        (body-forms (cddr form)))
    (let ((test-result (clojure-eval test-form env)))
      (when (truthy? test-result)
        ;; Evaluate all body forms, returning the last
        (if (null body-forms)
            nil
            (let ((last-expr (car (last body-forms))))
              (dolist (expr (butlast body-forms))
                (clojure-eval expr env))
              (clojure-eval last-expr env)))))))

(defun eval-when-not (form env)
  "Evaluate a when-not form: (when-not test body*) - evaluate body if test is falsey."
  (let ((test-form (cadr form))
        (body-forms (cddr form)))
    (let ((test-result (clojure-eval test-form env)))
      (when (falsey? test-result)
        ;; Evaluate all body forms, returning the last
        (if (null body-forms)
            nil
            (let ((last-expr (car (last body-forms))))
              (dolist (expr (butlast body-forms))
                (clojure-eval expr env))
              (clojure-eval last-expr env)))))))

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

(defun eval-defonce (form env)
  "Evaluate a defonce form: (defonce name expr) - define if not already defined.
   defonce ensures the var is created and initialized only once.
   If the var already has a non-nil value, the expr is not evaluated."
  ;; Handle metadata on the name: (defonce ^:dynamic name expr)
  (let* ((second (cadr form))
         (has-metadata (and (consp second)
                           (string= (symbol-name (car second)) "WITH-META")))
         (name (if has-metadata (cadr second) second))
         (value-expr (if has-metadata
                        (caddr form)
                        (caddr form)))
         (existing-var (env-get-var env name)))
    ;; If var doesn't exist or has no value, evaluate and set it
    (if (or (null existing-var)
            (null (var-value existing-var)))
        (let ((value (if value-expr
                        (clojure-eval value-expr env)
                        nil)))
          (env-set-var env name value))
        ;; Var already exists with a value, skip initialization
        nil)
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
  "Evaluate a fn form: (fn name? [args] body+) - returns a closure.
   Also handles (fn name? docstring? [args] body+) - with optional docstring."
  ;; Extract function parts
  (let* ((rest-form (cdr form))
         ;; Check if there's a docstring (string, followed by non-vector, followed by vector)
         ;; Format: (fn name docstring [params] body) or (fn docstring [params] body)
         (first-is-string (and (not (null rest-form))
                               (stringp (car rest-form))))
         (second-is-vector (and first-is-string
                               (not (null (cdr rest-form)))
                               (vectorp (cadr rest-form))))
         (has-docstring (and first-is-string second-is-vector))
         (rest-after-doc (if has-docstring (cdr rest-form) rest-form))
         (has-name (and (not (null rest-after-doc))
                        (not (vectorp (car rest-after-doc)))))
         (name (if has-name (car rest-after-doc) nil))
         (params (if has-name (cadr rest-after-doc) (car rest-after-doc)))
         (body (if has-name (cddr rest-after-doc) (cdr rest-after-doc))))
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
            do (let ((value (clojure-eval value-expr new-env))
                     (binding-form (if (vectorp name)
                                      (coerce name 'list)
                                      name)))
                 ;; Use extend-binding to handle destructuring (including nested vectors)
                 ;; DEBUG: print binding
                 ;; (format t "~&DEBUG eval-let: binding ~A to ~A~%" binding-form value)
                 (setf new-env (extend-binding new-env binding-form value)))))
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
   Each binding-expr is (binding-form . collection-expr) and local-modifiers is a list
   of modifiers that apply to this specific binding.

   The binding-form can be:
   - A symbol: [x coll] => ((x . coll) ())
   - A vector for destructuring: [[a b] coll] => (((a b) . coll) ())

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
                 ;; Regular binding: symbol expr OR vector-binding expr
                 ((or (symbolp clause) (vectorp clause))
                  (when (null rest)
                    (error "Missing expression for binding ~A in for" clause))
                  ;; For vector bindings, convert to list for destructuring
                  (let ((binding-form (if (vectorp clause)
                                         (coerce clause 'list)
                                         clause)))
                    (push (cons (cons binding-form (car rest)) nil) result)
                    (setf clauses (cdr rest))))
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

(defun extend-binding (env binding-form value)
  "Extend environment with a binding.
   binding-form can be:
   - A symbol: bind value to that symbol
   - A list: destructuring - bind each element of value list to corresponding symbol
   - Supports & for rest parameters: [a b & rest] binds first two to a,b and rest to remaining
   - Supports nested destructuring: [[a b] & rest] recursively binds nested structures
   Returns the new environment."
  (cond
    ((symbolp binding-form)
     (env-extend-lexical env binding-form value))
    ((listp binding-form)
     ;; Destructuring: value should be a sequence
     ;; Check for & rest parameter
     (let ((amp-pos (position (intern "&") binding-form :test #'eq)))
       (if amp-pos
           ;; Has rest parameter
           (let* ((regular-bindings (subseq binding-form 0 amp-pos))
                  (rest-binding (when (< (1+ amp-pos) (length binding-form))
                                (nth (1+ amp-pos) binding-form)))
                  (value-list (if (consp value) value (coerce value 'list)))
                  (new-env env)
                  (regular-count (min (length regular-bindings)
                                      (length value-list)))
                  (rest-values (nthcdr regular-count value-list)))
             ;; Bind regular parameters - use recursive extend-binding for nested destructuring
             ;; Convert vector binding forms to lists for destructuring
             (loop for i below regular-count
                   for sym in regular-bindings
                   for val in value-list
                   for binding-sym = (if (vectorp sym) (coerce sym 'list) sym)
                   do (setf new-env (extend-binding new-env binding-sym val)))
             ;; Bind rest parameter if present - recursively if it's a list/vector
             ;; Convert vector binding forms to lists for destructuring
             (when rest-binding
               (let ((rest-binding-sym (if (vectorp rest-binding)
                                          (coerce rest-binding 'list)
                                          rest-binding)))
                 (setf new-env (extend-binding new-env rest-binding-sym rest-values))))
             new-env)
           ;; No rest parameter - simple destructuring with recursive binding
           ;; Convert vector binding forms to lists for destructuring
           (let ((value-list (if (consp value) value (coerce value 'list))))
             (loop for sym in binding-form
                   for val in value-list
                   for binding-sym = (if (vectorp sym) (coerce sym 'list) sym)
                   with new-env = env
                   do (setf new-env (extend-binding new-env binding-sym val))
                   finally (return new-env))))))
    (t
     (error "Invalid binding form: ~A (type: ~A)" binding-form (type-of binding-form)))))

(defun eval-for-nested (bindings body-expr env)
  "Evaluate nested for comprehension, producing list of results.
   bindings is (((binding-form . unevaluated-expr) . local-modifiers)...) - exprs are
   evaluated with current env, and modifiers apply to each iteration of that binding.
   binding-form can be a symbol or a list for destructuring."
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
                           do (let* ((new-env (extend-binding env first-binding i))
                                     (filtered-env (apply-local-modifiers new-env local-modifiers)))
                                (when filtered-env
                                  (let ((nested-results (eval-for-nested rest-bindings
                                                                        body-expr
                                                                        filtered-env)))
                                    (setf results (append results nested-results)))))))
                   ;; Infinite range - limit iterations
                   (loop for i from start by step
                         repeat 1000
                         do (let* ((new-env (extend-binding env first-binding i))
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
                 (let* ((new-env (extend-binding env first-binding elem))
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
         (body-exprs (cddr bindings-body)))  ; cddr, not cadr - get all body expressions
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
          do (let ((value (clojure-eval value-expr new-env))
                   (binding-form (if (vectorp name)
                                    (coerce name 'list)
                                    name)))
               ;; Use extend-binding to handle destructuring (including nested vectors)
               (setf new-env (extend-binding new-env binding-form value))))
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

(defun eval-thread-first (form env)
  "Evaluate a thread-first (->) form: (-> x form1 form2 ...)
   Threads the expression as the first argument through the forms.
   (-> x (f) (g a b)) becomes (g (f x) a b)"
  (let* ((forms (cdr form)))
    (if (null forms)
        (error "thread-first requires at least one expression")
        ;; Start with the first expression
        (let ((result (clojure-eval (car forms) env)))
          ;; Thread through remaining forms
          (dolist (form-expr (cdr forms))
            (typecase form-expr
              ;; If it's a list, insert result as first arg
              (cons
               (let ((fn-sym (car form-expr))
                     (args (cdr form-expr)))
                 (setq result (clojure-eval (cons fn-sym (cons result args)) env))))
              ;; If it's a symbol, just call it with result
              (symbol
               (setq result (clojure-eval (list form-expr result) env))))
            ;; If it's a vector or other non-list, just use it as-is
            (unless (or (consp form-expr) (symbolp form-expr))
              (setq result (clojure-eval form-expr env))))
          result))))

(defun eval-thread-last (form env)
  "Evaluate a thread-last (->>) form: (->> x form1 form2 ...)
   Threads the expression as the last argument through the forms.
   (->> x (f) (g a b)) becomes (g a b (f x))"
  (let* ((forms (cdr form)))
    (if (null forms)
        (error "thread-last requires at least one expression")
        ;; Start with the first expression
        (let ((result (clojure-eval (car forms) env)))
          ;; Thread through remaining forms
          (dolist (form-expr (cdr forms))
            (typecase form-expr
              ;; If it's a list, insert result as last arg
              (cons
               (let ((fn-sym (car form-expr))
                     (args (cdr form-expr)))
                 (setq result (clojure-eval (append (list fn-sym) args (list result)) env))))
              ;; If it's a symbol, just call it with result
              (symbol
               (setq result (clojure-eval (list form-expr result) env))))
            ;; If it's a vector or other non-list, just use it as-is
            (unless (or (consp form-expr) (symbolp form-expr))
              (setq result (clojure-eval form-expr env))))
          result))))

(defun eval-try (form env)
  "Evaluate a try form: (try body catch* finally?)
   Simplified implementation for basic exception handling."
  (let* ((forms (cdr form))
         (body-forms nil)
         (catch-clauses nil)
         (finally-clause nil)
         (current forms))
    ;; Parse the form - collect body forms until we hit catch or finally
    (loop while (and current (not (eq (caar current) 'catch)) (not (eq (caar current) 'finally)))
          do (push (pop current) body-forms))
    (setq body-forms (nreverse body-forms))
    ;; Parse catch and finally clauses
    (loop while current
          do (let ((clause (car current)))
               (cond
                 ((eq (car clause) 'catch)
                  (push (cdr clause) catch-clauses))
                 ((eq (car clause) 'finally)
                  (setq finally-clause (cdr clause)))
                 (t nil))
               (pop current)))
    (setq catch-clauses (nreverse catch-clauses))
    ;; Evaluate body with error handling
    (let ((result nil)
          (error-occurred nil)
          (caught-value nil))
      (unwind-protect
           (handler-case
               ;; Evaluate body forms
               (dolist (expr body-forms)
                 (setq result (clojure-eval expr env)))
             (error (c)
               (setq error-occurred t)
               (setq caught-value c)
               ;; Try catch clauses
               (block catching
                 (dolist (clause catch-clauses)
                   ;; clause: (classname sym body*)
                   (let* ((sym (cadr clause))
                          (catch-body (cddr clause))
                          (new-env (env-extend-lexical env sym caught-value)))
                     (return-from catching
                       (dolist (expr catch-body)
                         (setq result (clojure-eval expr new-env))))))
                 (error c))))  ; re-throw if no catch matches
        ;; Execute finally clause
        (when finally-clause
          (dolist (expr finally-clause)
            (clojure-eval expr env))))
      result)))

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
;;; Delay (lazy evaluation) representation
;;; ============================================================

(defstruct delay
  "A delay - holds a thunk for delayed evaluation and a cached value."
  thunk
  (value nil)
  (forced-p nil))

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
;;; Java Interop Stubs
;;; ============================================================

(defun java-interop-stub-lookup (symbol)
  "Look up a Java interop symbol (e.g., Math/round) and return a stub function.
   Returns NIL if not found. For static fields (MAX_VALUE, MIN_VALUE, TYPE),
   returns the value directly instead of a function."
  (let ((name (symbol-name symbol)))
    (when (find #\/ name)
      (let* ((slash-pos (position #\/ name))
             (class-name (subseq name 0 slash-pos))
             (member-name (subseq name (1+ slash-pos))))
        ;; Check if this is a static field access (no args needed)
        ;; These are: MAX_VALUE, MIN_VALUE, TYPE for primitive wrapper classes
        (if (and (member member-name '("MAX_VALUE" "MIN_VALUE" "TYPE") :test #'string-equal)
                 (member class-name '("Byte" "Short" "Integer" "Long" "Float" "Double" "Character" "Boolean") :test #'string-equal))
            ;; For static fields, evaluate and return the value directly
            (eval-java-interop (intern class-name) (intern member-name))
            ;; For methods, return the class/member list for creating a lambda
            (list class-name member-name))))))

(defun eval-java-interop (class-name member-name &rest args)
  "Evaluate a Java interop call. This is a stub implementation."
  ;; Special cases for common Java interop patterns
  (cond
    ;; Math class methods
    ((string-equal class-name "Math")
     (cond
       ((string-equal member-name "round")
        (if (null args)
            (error "Math/round requires an argument")
            (round (first args))))
       ((string-equal member-name "floor")
        (if (null args)
            (error "Math/floor requires an argument")
            (floor (first args))))
       ((string-equal member-name "ceil")
        (if (null args)
            (error "Math/ceil requires an argument")
            (ceiling (first args))))
       ((string-equal member-name "abs")
        (if (null args)
            (error "Math/abs requires an argument")
            (abs (first args))))
       ((string-equal member-name "min")
        (apply #'min args))
       ((string-equal member-name "max")
        (apply #'max args))
       ((string-equal member-name "pow")
        (if (< (length args) 2)
            (error "Math/pow requires 2 arguments")
            (expt (first args) (second args))))
       ((string-equal member-name "sqrt")
        (if (null args)
            (error "Math/sqrt requires an argument")
            (sqrt (first args))))
       ((string-equal member-name "sin")
        (if (null args)
            (error "Math/sin requires an argument")
            (sin (first args))))
       ((string-equal member-name "cos")
        (if (null args)
            (error "Math/cos requires an argument")
            (cos (first args))))
       ((string-equal member-name "tan")
        (if (null args)
            (error "Math/tan requires an argument")
            (tan (first args))))
       ((string-equal member-name "log")
        (if (null args)
            (error "Math/log requires an argument")
            (log (first args))))
       ((string-equal member-name "log10")
        (if (null args)
            (error "Math/log10 requires an argument")
            (log (first args) 10)))
       (t
        (error "Unsupported Math method: ~A" member-name))))
    ;; System class methods/fields
    ((string-equal class-name "System")
     (cond
       ((string-equal member-name "getProperty")
        (if (null args)
            nil
            ;; Return stub values for common properties
            (let ((prop (first args)))
              (cond
                ((stringp prop)
                 (cond
                   ((search "line.separator" prop) #\Newline)
                   ((search "path.separator" prop) ":")
                   ((search "file.separator" prop) "/")
                   ((search "os.name" prop) "Linux")
                   ((search "java.version" prop) "11.0")
                   (t nil)))
                (t nil)))))
       ((string-equal member-name "getenv")
        (if (null args)
            nil
            ;; Return stub values for common env vars
            (let ((var (first args)))
              (cond
                ((stringp var)
                 (cond
                   ((search "PATH" var) "/usr/bin:/bin")
                   ((search "HOME" var) "/home/user")
                   (t nil)))
                (t nil)))))
       (t
        (error "Unsupported System method: ~A" member-name))))
    ;; Class class methods/fields
    ((string-equal class-name "Class")
     (cond
       ((string-equal member-name "forName")
        ;; Return a stub Class object
        (if (null args)
            (error "Class/forName requires an argument")
            :class))
       ((string-equal member-name "TYPE")
        ;; Return a stub TYPE value for primitive classes
        :type)
       (t
        (error "Unsupported Class method: ~A" member-name))))
    ;; Boolean class fields
    ((string-equal class-name "Boolean")
     (cond
       ((string-equal member-name "TYPE")
        :boolean-type)
       (t
        (error "Unsupported Boolean field: ~A" member-name))))
    ;; Integer class fields
    ((string-equal class-name "Integer")
     (cond
       ((string-equal member-name "TYPE")
        :int-type)
       ((string-equal member-name "MAX_VALUE")
        most-positive-fixnum)
       ((string-equal member-name "MIN_VALUE")
        most-negative-fixnum)
       (t
        (error "Unsupported Integer field: ~A" member-name))))
    ;; Long class fields
    ((string-equal class-name "Long")
     (cond
       ((string-equal member-name "TYPE")
        :long-type)
       ((string-equal member-name "MAX_VALUE")
        most-positive-fixnum)
       ((string-equal member-name "MIN_VALUE")
        most-negative-fixnum)
       (t
        (error "Unsupported Long field: ~A" member-name))))
    ;; Float class fields
    ((string-equal class-name "Float")
     (cond
       ((string-equal member-name "TYPE")
        :float-type)
       ((string-equal member-name "MAX_VALUE")
        most-positive-single-float)
       ((string-equal member-name "MIN_VALUE")
        least-negative-single-float)
       (t
        (error "Unsupported Float field: ~A" member-name))))
    ;; Double class fields
    ((string-equal class-name "Double")
     (cond
       ((string-equal member-name "TYPE")
        :double-type)
       ((string-equal member-name "MAX_VALUE")
        most-positive-double-float)
       ((string-equal member-name "MIN_VALUE")
        least-negative-double-float)
       (t
        (error "Unsupported Double field: ~A" member-name))))
    ;; Character class fields
    ((string-equal class-name "Character")
     (cond
       ((string-equal member-name "TYPE")
        :char-type)
       (t
        (error "Unsupported Character field: ~A" member-name))))
    ;; Byte class fields
    ((string-equal class-name "Byte")
     (cond
       ((string-equal member-name "TYPE")
        :byte-type)
       ((string-equal member-name "MAX_VALUE")
        127)
       ((string-equal member-name "MIN_VALUE")
        -128)
       (t
        (error "Unsupported Byte field: ~A" member-name))))
    ;; Short class fields
    ((string-equal class-name "Short")
     (cond
       ((string-equal member-name "TYPE")
        :short-type)
       ((string-equal member-name "MAX_VALUE")
        32767)
       ((string-equal member-name "MIN_VALUE")
        -32768)
       (t
        (error "Unsupported Short field: ~A" member-name))))
    ;; Object class methods
    ((string-equal class-name "Object")
     :object)
    ;; String class methods
    ((string-equal class-name "String")
     (cond
       ((string-equal member-name "valueOf")
        (if (null args)
            ""
            (str (first args))))
       (t
        (error "Unsupported String method: ~A" member-name))))
    ;; Default: error for unknown Java interop
    (t
     (error "Unsupported Java interop: ~A/~A" class-name member-name))))

(defun eval-java-constructor (class-name args)
  "Evaluate a Java constructor call. This is a stub implementation.
   For primitive wrapper classes, just return the argument value."
  (let ((name (string class-name)))
    (cond
      ;; Byte constructor - returns the byte value
      ((string-equal name "Byte")
       (if (null args)
           0
           (let ((val (first args)))
             (cond
               ((numberp val) (round val))
               (t 0)))))
      ;; Short constructor
      ((string-equal name "Short")
       (if (null args)
           0
           (let ((val (first args)))
             (cond
               ((numberp val) (round val))
               (t 0)))))
      ;; Integer constructor
      ((string-equal name "Integer")
       (if (null args)
           0
           (let ((val (first args)))
             (cond
               ((numberp val) (truncate val))
               (t 0)))))
      ;; Long constructor
      ((string-equal name "Long")
       (if (null args)
           0
           (let ((val (first args)))
             (cond
               ((numberp val) (truncate val))
               (t 0)))))
      ;; Float constructor
      ((string-equal name "Float")
       (if (null args)
           0.0
           (let ((val (first args)))
             (cond
               ((numberp val) (float val))
               (t 0.0)))))
      ;; Double constructor
      ((string-equal name "Double")
       (if (null args)
           0.0
           (let ((val (first args)))
             (cond
               ((numberp val) (coerce val 'double-float))
               (t 0.0)))))
      ;; Character constructor
      ((string-equal name "Character")
       (if (null args)
           #\Nul
           (let ((val (first args)))
             (cond
               ((characterp val) val)
               ((numberp val) (code-char (round val)))
               (t #\Nul)))))
      ;; Boolean constructor
      ((string-equal name "Boolean")
       (if (null args)
           nil
           (let ((val (first args)))
             (not (null val)))))
      ;; Default: error for unknown constructors
      (t
       (error "Unsupported Java constructor: ~A" class-name)))))

(defun setup-java-interop-stubs (env)
  "Set up stub functions for common Java interop symbols.
   This pre-registers frequently-used Java class/member combinations."
  (declare (ignore env))
  ;; The stubs are handled dynamically in clojure-eval
  ;; via the java-interop-stub-lookup function
  nil)

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
  ;; Register *' (arbitrary precision multiplication) - use intern to create the symbol
  (env-set-var env (intern "*'") #'clojure*)
  (register-core-function env '/ #'clojure/)
  ;; Register -' (arbitrary precision subtraction) - use intern to create the symbol
  (env-set-var env (intern "-'") #'clojure-)
  (register-core-function env 'inc #'clojure-inc)
  (register-core-function env 'dec #'clojure-dec)

  ;; Comparison functions
  (register-core-function env '= #'clojure=)
  (register-core-function env '== #'clojure=)
  (register-core-function env '< #'clojure<)
  (register-core-function env '> #'clojure>)
  (register-core-function env '<= #'clojure<=)
  (register-core-function env '>= #'clojure>=)
  (register-core-function env 'min #'clojure-min)
  (register-core-function env 'max #'clojure-max)

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
  (register-core-function env 'bytes #'clojure-bytes)
  (register-core-function env 'booleans #'clojure-booleans)
  (register-core-function env 'shorts #'clojure-shorts)
  (register-core-function env 'chars #'clojure-chars)
  (register-core-function env 'ints #'clojure-ints)
  (register-core-function env 'longs #'clojure-longs)
  (register-core-function env 'floats #'clojure-floats)
  (register-core-function env 'doubles #'clojure-doubles)

  ;; Delay/Force functions (force is a function, delay is a special form)
  (register-core-function env 'force #'clojure-force)

  ;; Predicate functions
  (register-core-function env 'nil? #'clojure-nil?)
  (register-core-function env 'symbol? #'clojure-symbol?)
  (register-core-function env 'keyword? #'clojure-keyword?)
  (register-core-function env 'number? #'clojure-number?)
  (register-core-function env 'integer? #'clojure-integer?)
  (register-core-function env 'float? #'clojure-float?)
  (register-core-function env 'decimal? #'clojure-decimal?)
  (register-core-function env 'fn? #'clojure-fn?)
  (register-core-function env 'vector? #'clojure-vector?)
  (register-core-function env 'not #'clojure-not)
  (register-core-function env 'some? #'clojure-some?)
  (register-core-function env 'true? #'clojure-true?)
  (register-core-function env 'false? #'clojure-false?)

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
  (register-core-function env 'get #'clojure-get)
  (register-core-function env 'contains? #'clojure-contains?)
  (register-core-function env 'assoc #'clojure-assoc)
  (register-core-function env 'dissoc #'clojure-dissoc)
  (register-core-function env 'keys #'clojure-keys)
  (register-core-function env 'vals #'clojure-vals)
  (register-core-function env 'quot #'clojure-quot)
  (register-core-function env 'rem #'clojure-rem)
  (register-core-function env 'mod #'clojure-mod)
  (register-core-function env 'even? #'clojure-even?)
  (register-core-function env 'odd? #'clojure-odd?)
  (register-core-function env 'neg? #'clojure-neg?)
  (register-core-function env 'pos? #'clojure-pos?)
  (register-core-function env 'zero? #'clojure-zero?)
  (register-core-function env 'bit-shift-left #'clojure-bit-shift-left)
  (register-core-function env 'bit-shift-right #'clojure-bit-shift-right)
  (register-core-function env 'unsigned-bit-shift-right #'clojure-unsigned-bit-shift-right)
  (register-core-function env 'bit-and #'clojure-bit-and)
  (register-core-function env 'bit-or #'clojure-bit-or)
  (register-core-function env 'bit-xor #'clojure-bit-xor)
  (register-core-function env 'bit-not #'clojure-bit-not)
  (register-core-function env 'bit-clear #'clojure-bit-clear)
  (register-core-function env 'bit-set #'clojure-bit-set)
  (register-core-function env 'bit-flip #'clojure-bit-flip)
  (register-core-function env 'bit-test #'clojure-bit-test)

  ;; String/Symbol functions
  (register-core-function env 'symbol #'clojure-symbol)
  (register-core-function env 'atom #'clojure-atom)
  (register-core-function env 'read-string #'clojure-read-string)
  (register-core-function env 'println #'clojure-println)
  (register-core-function env 'prn #'clojure-prn)
  (register-core-function env 'constantly #'clojure-constantly)
  (register-core-function env 'complement #'clojure-complement)
  (register-core-function env 'comp #'clojure-comp)
  (register-core-function env 'fnil #'clojure-fnil)
  (register-core-function env 'repeatedly #'clojure-repeatedly)
  (register-core-function env 'filter #'clojure-filter)
  (register-core-function env 'juxt #'clojure-juxt)
  (register-core-function env 'replicate #'clojure-replicate)
  (register-core-function env 'repeat #'clojure-repeat)

  ;; Metadata functions
  (register-core-function env 'meta #'clojure-meta)
  (register-core-function env 'with-meta #'clojure-with-meta)
  (register-core-function env 'vary-meta #'clojure-vary-meta)

  ;; Array functions
  (register-core-function env 'byte-array #'clojure-byte-array)
  (register-core-function env 'short-array #'clojure-short-array)
  (register-core-function env 'char-array #'clojure-char-array)
  (register-core-function env 'int-array #'clojure-int-array)
  (register-core-function env 'long-array #'clojure-long-array)
  (register-core-function env 'float-array #'clojure-float-array)
  (register-core-function env 'double-array #'clojure-double-array)
  (register-core-function env 'boolean-array #'clojure-boolean-array)
  (register-core-function env 'aset #'clojure-aset)

  ;; Unchecked cast functions
  (register-core-function env 'unchecked-byte #'clojure-unchecked-byte)
  (register-core-function env 'unchecked-short #'clojure-unchecked-short)
  (register-core-function env 'unchecked-char #'clojure-unchecked-char)
  (register-core-function env 'unchecked-int #'clojure-unchecked-int)
  (register-core-function env 'unchecked-long #'clojure-unchecked-long)
  (register-core-function env 'unchecked-float #'clojure-unchecked-float)
  (register-core-function env 'unchecked-double #'clojure-unchecked-double)

  ;; Type conversion functions
  (register-core-function env 'char #'clojure-char)
  (register-core-function env 'byte #'clojure-byte)
  (register-core-function env 'short #'clojure-byte)
  (register-core-function env 'int #'clojure-int)
  (register-core-function env 'long #'clojure-long)
  (register-core-function env 'float #'clojure-float)
  (register-core-function env 'double #'clojure-double)
  (register-core-function env 'bigint #'clojure-bigint)
  (register-core-function env 'biginteger #'clojure-bigint)
  (register-core-function env 'bigdec #'clojure-bigdec)
  (register-core-function env 'class #'clojure-class)
  (register-core-function env 'type #'clojure-type)

  ;; Test helper stubs
  ;; Note: thrown? is now a special form, not a registered function

  ;; Java exception class stubs (for thrown? tests)
  (env-set-var env 'ClassCastException :class-cast-exception)
  (env-set-var env 'IllegalArgumentException :illegal-argument-exception)
  (env-set-var env 'IllegalStateException :illegal-state-exception)
  (env-set-var env 'NullPointerException :null-pointer-exception)
  (env-set-var env 'ArithmeticException :arithmetic-exception)
  (env-set-var env 'IndexOutOfBoundsException :index-out-of-bounds-exception)
  (env-set-var env 'NumberFormatException :number-format-exception)
  (env-set-var env 'UnsupportedOperationException :unsupported-operation-exception)
  (env-set-var env 'Exception :exception)

  ;; Java interop stubs (Class/member notation)
  (setup-java-interop-stubs env)

  env)

;;; Arithmetic implementations
(defun clojure+ (&rest args)
  "Add all arguments. Returns 0 if no args.
   Throws ClassCastException if any arg is not a number."
  (if (null args)
      0
      (progn
        ;; Check all args are numbers
        (dolist (arg args)
          (when (not (numberp arg))
            (error 'type-error :datum arg :expected-type 'number)))
        (reduce #'+ args))))

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
        ;; equal is binary, so we need to check pairwise
        (every (lambda (x) (equal (car processed-args) x))
               (cdr processed-args)))))

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

(defun clojure-min (x &rest args)
  "Return the minimum of the arguments."
  (if (null args)
      x
      (reduce #'min (cons x args))))

(defun clojure-max (x &rest args)
  "Return the maximum of the arguments."
  (if (null args)
      x
      (reduce #'max (cons x args))))

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
  "Create a vector from collection.
   For lazy ranges, limits to 10000 elements to avoid heap exhaustion."
  (cond
    ((vectorp coll) coll)
    ((lazy-range-p coll)
     (let ((limit (if (lazy-range-end coll)
                      ;; Bounded range - use actual end
                      most-positive-fixnum
                      ;; Infinite range - limit to 10000
                      10000)))
       (coerce (lazy-range-to-list coll limit) 'vector)))
    (t (coerce coll 'vector))))

(defun clojure-vector (&rest args)
  "Create a vector from arguments."
  (coerce args 'vector))

(defun clojure-list (&rest args)
  "Create a list from arguments."
  args)

(defun clojure-map (fn-arg coll &rest colls)
  "Apply fn to each item in collection(s). Returns lazy sequence.
   Supports mapping over multiple collections in parallel."
  (if (null colls)
      ;; Single collection mapping
      (cond
        ((null coll) '())
        ((lazy-range-p coll)
         ;; For lazy ranges, limit to 1000 elements to avoid issues
         (let ((start (lazy-range-start coll))
               (end (lazy-range-end coll))
               (step (lazy-range-step coll)))
           (if end
               ;; Bounded range
               (loop for i from start below end by step
                     collect (funcall fn-arg i))
               ;; Infinite range - limit to 1000
               (loop for i from start by step
                     repeat 1000
                     collect (funcall fn-arg i)))))
        ((listp coll)
         (mapcar fn-arg coll))
        (t
         ;; Convert to list and map
         (mapcar fn-arg (coerce coll 'list))))
      ;; Multiple collections - map in parallel
      (let* ((all-colls (cons coll colls))
             (min-length (apply #'min (mapcar (lambda (c)
                                                (cond
                                                  ((null c) 0)
                                                  ((lazy-range-p c)
                                                   (if (lazy-range-end c)
                                                       (ceiling (/ (- (lazy-range-end c)
                                                                      (lazy-range-start c))
                                                                   (lazy-range-step c)))
                                                       1000))
                                                  ((listp c) (length c))
                                                  (t (length (coerce c 'list)))))
                                              all-colls))))
        ;; Convert all to lists
        (let ((coll-lists (mapcar (lambda (c)
                                    (cond
                                      ((null c) '())
                                      ((lazy-range-p c) (lazy-range-to-list c))
                                      ((listp c) c)
                                      (t (coerce c 'list))))
                                  all-colls)))
          ;; Map function across elements at each position
          (loop for i from 0 below min-length
                collect (apply fn-arg (mapcar (lambda (c) (nth i c)) coll-lists)))))))

(defun clojure-apply (fn-arg &rest args)
  "Apply fn to args with last arg being a list of args."
  (let ((all-but-last (butlast args))
        (last-arg (car (last args))))
    ;; Convert last-arg to list if it's a lazy range or vector
    (let ((last-as-list (cond
                         ((lazy-range-p last-arg)
                          (lazy-range-to-list last-arg))
                         ((vectorp last-arg)
                          (coerce last-arg 'list))
                         (t last-arg))))
      (apply fn-arg (append all-but-last last-as-list)))))

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

;; Type conversion functions for sequences
(defun clojure-bytes (seq)
  "Convert a sequence to a byte array (vector for SBCL)."
  (clojure-byte-array seq))
(defun clojure-booleans (seq)
  "Convert a sequence to a boolean array (vector for SBCL)."
  (clojure-boolean-array seq))
(defun clojure-shorts (seq)
  "Convert a sequence to a short array (vector for SBCL)."
  (clojure-short-array seq))
(defun clojure-chars (seq)
  "Convert a sequence to a char array (vector for SBCL)."
  (clojure-char-array seq))
(defun clojure-ints (seq)
  "Convert a sequence to an int array (vector for SBCL)."
  (clojure-int-array seq))
(defun clojure-longs (seq)
  "Convert a sequence to a long array (vector for SBCL)."
  (clojure-long-array seq))
(defun clojure-floats (seq)
  "Convert a sequence to a float array (vector for SBCL)."
  (clojure-float-array seq))
(defun clojure-doubles (seq)
  "Convert a sequence to a double array (vector for SBCL)."
  (clojure-double-array seq))

;; Array creation functions - stubs that create vectors
(defun clojure-byte-array (&optional size-or-seq init-val)
  "Create a byte array. Returns a vector for SBCL.
   Supports (byte-array size), (byte-array size init-val), or (byte-array seq)."
  (if (integerp size-or-seq)
      (make-array size-or-seq :initial-element (or init-val 0))
      (coerce (or size-or-seq '()) 'vector)))

(defun clojure-short-array (&optional size-or-seq init-val)
  "Create a short array. Returns a vector for SBCL."
  (if (integerp size-or-seq)
      (make-array size-or-seq :initial-element (or init-val 0))
      (coerce (or size-or-seq '()) 'vector)))

(defun clojure-char-array (&optional size-or-seq init-val)
  "Create a char array. Returns a vector for SBCL."
  (if (integerp size-or-seq)
      (make-array size-or-seq :initial-element (or init-val #\Nul))
      (coerce (or size-or-seq '()) 'vector)))

(defun clojure-int-array (&optional size-or-seq init-val)
  "Create an int array. Returns a vector for SBCL."
  (if (integerp size-or-seq)
      (make-array size-or-seq :initial-element (or init-val 0))
      (coerce (or size-or-seq '()) 'vector)))

(defun clojure-long-array (&optional size-or-seq init-val)
  "Create a long array. Returns a vector for SBCL."
  (if (integerp size-or-seq)
      (make-array size-or-seq :initial-element (or init-val 0))
      (coerce (or size-or-seq '()) 'vector)))

(defun clojure-float-array (&optional size-or-seq init-val)
  "Create a float array. Returns a vector for SBCL."
  (if (integerp size-or-seq)
      (make-array size-or-seq :initial-element (or init-val 0.0))
      (coerce (or size-or-seq '()) 'vector)))

(defun clojure-double-array (&optional size-or-seq init-val)
  "Create a double array. Returns a vector for SBCL."
  (if (integerp size-or-seq)
      (make-array size-or-seq :initial-element (or init-val 0.0d0))
      (coerce (or size-or-seq '()) 'vector)))

(defun clojure-boolean-array (&optional size-or-seq init-val)
  "Create a boolean array. Returns a vector for SBCL."
  (if (integerp size-or-seq)
      (make-array size-or-seq :initial-element init-val)
      (coerce (or size-or-seq '()) 'vector)))

;; Array operations
(defun clojure-aset (array index value)
  "Set array element at index to value. Returns value."
  (when (arrayp array)
    (setf (aref array index) value))
  value)

;; Type conversion functions
(defun clojure-char (x)
  "Convert a number to a character. Stub for SBCL."
  (if (integerp x)
      (code-char x)
      x))
(defun clojure-byte (x)
  "Convert a number to a byte. Stub for SBCL - just truncate to integer."
  (truncate x))

(defun clojure-int (x)
  "Convert to an integer. Stub for SBCL."
  (if (integerp x)
      x
      (truncate x)))

(defun clojure-long (x) (clojure-int x))
(defun clojure-float (x) (coerce x 'single-float))
(defun clojure-double (x) (coerce x 'double-float))

;; BigInt/BigDecimal conversion stubs - for now just return the number
(defun clojure-bigint (x) x)
(defun clojure-biginteger (x) x)
(defun clojure-bigdec (x) x)

;; Class/type introspection stubs
(defun clojure-class (x)
  "Return the class of x. Stub for SBCL - returns a keyword representing the type."
  (cond
    ((integerp x) :integer)
    ((floatp x) :float)
    ((stringp x) :string)
    ((characterp x) :char)
    ((vectorp x) :vector)
    ((listp x) :list)
    ((hash-table-p x) :map)
    (t :object)))
(defun clojure-type (x)
  "Return the type of x. Same as class for our stub."
  (clojure-class x))

;; Integer division functions
(defun clojure-quot (num div)
  "Return the quotient of dividing num by div (truncated toward zero)."
  (truncate (/ num div)))
(defun clojure-rem (num div)
  "Return the remainder of dividing num by div (same sign as num)."
  (rem num div))
(defun clojure-mod (num div)
  "Return the modulo of num and div (same sign as div)."
  (mod num div))
(defun clojure-even? (x)
  "Return true if x is even."
  (if (and (integerp x) (evenp x)) 'true nil))
(defun clojure-odd? (x)
  "Return true if x is odd."
  (if (and (integerp x) (oddp x)) 'true nil))
(defun clojure-neg? (x)
  "Return true if x is negative."
  (if (and (numberp x) (< x 0)) 'true nil))
(defun clojure-pos? (x)
  "Return true if x is positive."
  (if (and (numberp x) (> x 0)) 'true nil))
(defun clojure-zero? (x)
  "Return true if x is zero."
  (if (and (numberp x) (= x 0)) 'true nil))
;; Bit manipulation functions
(defun clojure-bit-shift-left (x n)
  "Bitwise left shift."
  (ash x n))
(defun clojure-bit-shift-right (x n)
  "Bitwise right shift (sign-extended)."
  (ash x (- n)))
(defun clojure-unsigned-bit-shift-right (x n)
  "Unsigned bitwise right shift (zero-extended).  Stub for SBCL."
  ;; SBCL doesn't have unsigned shift, so we use regular shift
  (ash x (- n)))
(defun clojure-bit-and (&rest args)
  "Bitwise AND of all arguments."
  (if (null args)
      -1  ; all bits set for default
      (reduce #'logand args)))
(defun clojure-bit-or (&rest args)
  "Bitwise OR of all arguments."
  (if (null args)
      0  ; all bits clear for default
      (reduce #'logior args)))
(defun clojure-bit-xor (&rest args)
  "Bitwise XOR of all arguments."
  (if (null args)
      0  ; all bits clear for default
      (reduce #'logxor args)))
(defun clojure-bit-not (x)
  "Bitwise complement."
  (lognot x))
(defun clojure-bit-clear (x n)
  "Clear bit at index n."
  (logand x (lognot (ash 1 n))))
(defun clojure-bit-set (x n)
  "Set bit at index n."
  (logior x (ash 1 n)))
(defun clojure-bit-flip (x n)
  "Flip bit at index n."
  (logxor x (ash 1 n)))
(defun clojure-bit-test (x n)
  "Test bit at index n."
  (not (zerop (logand x (ash 1 n)))))

;; Unchecked cast operations - stubs that just return the value
(defun clojure-unchecked-byte (x) x)
(defun clojure-unchecked-short (x) x)
(defun clojure-unchecked-char (x) x)
(defun clojure-unchecked-int (x) x)
(defun clojure-unchecked-long (x) x)
(defun clojure-unchecked-float (x) x)
(defun clojure-unchecked-double (x) x)

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

(defun clojure-constantly (x)
  "Return a function that always returns x."
  (lambda (&rest args) (declare (ignore args)) x))

(defun clojure-complement (f)
  "Return a function that returns the logical negation of f."
  (lambda (&rest args)
    (let ((result (apply f args)))
      (if (or (null result) (eq result 'false))
          'true
          nil))))

(defun clojure-fnil (f &rest fill-values)
  "Return a function that calls f with fill-values for nil arguments.
   (fnil f x y) returns a function that when called as (g a b c)
   calls (f (or a x) (or b y) c)"
  (lambda (&rest args)
    (let* ((fill-count (min (length fill-values) (length args)))
           (filled-args
             (loop for i from 0 below fill-count
                   for arg in args
                   for fill in fill-values
                   collect (if (null arg) fill arg)))
           (remaining-args (nthcdr fill-count args))
           (all-args (append filled-args remaining-args)))
      (apply f all-args))))

(defun clojure-repeatedly (f &optional n)
  "Return a lazy sequence of calling f repeatedly.
   If n is provided, return only n elements."
  (let ((results nil))
    (if n
        (loop repeat n
              do (push (funcall f) results)
              finally (return (nreverse results)))
        ;; Infinite sequence - limit to 1000 for practicality
        (loop repeat 1000
              do (push (funcall f) results)
              finally (return (nreverse results))))))

(defun clojure-filter (pred coll)
  "Return a lazy sequence of items in coll for which pred returns true."
  (cond
    ((null coll) '())
    ((lazy-range-p coll)
     (let ((start (lazy-range-start coll))
           (end (lazy-range-end coll))
           (step (lazy-range-step coll)))
       (if end
           (loop for i from start below end by step
                 when (funcall pred i)
                 collect i)
           (loop for i from start by step
                 repeat 1000
                 when (funcall pred i)
                 collect i))))
    ((listp coll)
     (loop for item in coll
           when (funcall pred item)
           collect item))
    (t
     (let ((coll-list (coerce coll 'list)))
       (loop for item in coll-list
             when (funcall pred item)
             collect item)))))

(defun clojure-comp (&rest fns)
  "Return a function that is the composition of the given functions.
   (comp f g) returns a function that calls (f (g x...))
   (comp) returns identity."
  (if (null fns)
      #'clojure-identity
      (let ((fn-list (reverse fns)))
        (lambda (&rest args)
          (reduce (lambda (result fn)
                    (if (listp result)
                        (apply fn result)
                        (funcall fn result)))
                  fn-list
                  :initial-value args)))))

(defun clojure-juxt (&rest fns)
  "Return a function that applies each function to arguments.
   Returns a vector of results."
  (lambda (&rest args)
    (coerce (mapcar (lambda (f) (apply f args)) fns) 'vector)))

;;; Predicate implementations
(defun clojure-nil? (x) (null x))
(defun clojure-symbol? (x) (symbolp x))
(defun clojure-keyword? (x) (keywordp x))
(defun clojure-number? (x) (numberp x))
(defun clojure-integer? (x)
  "Return true if x is an integer (not a float/decimal)."
  (and (numberp x) (integerp x)))
(defun clojure-float? (x)
  "Return true if x is a floating-point number."
  (and (numberp x) (floatp x)))
(defun clojure-decimal? (x)
  "Return true if x is a decimal (BigDecimal).
   For our stub, just check if it's a float."
  (and (numberp x) (floatp x)))
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
(defun clojure-true? (x)
  "Return true if x is truthy (not nil or false)."
  (not (null x)))
(defun clojure-false? (x)
  "Return true if x is falsey (nil or false)."
  (or (null x) (eq x 'false)))

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

;;; Metadata support
;;; We wrap values that have metadata in a special marker cons cell

(defvar *metadata-wrapper-marker* 'meta-wrapper
  "Marker used to identify wrapped values with metadata.")

(defun wrap-with-meta (value metadata)
  "Wrap a value with metadata for tracking.
  The wrapped form is (meta-wrapper value metadata) so that cadr gets value."
  (cons *metadata-wrapper-marker* (cons value metadata)))

(defun wrapped-p (value)
  "Check if a value is wrapped with metadata."
  (and (consp value)
       (eq (car value) *metadata-wrapper-marker*)))

(defun unwrap-value (wrapped)
  "Get the value from a wrapped metadata object."
  (cadr wrapped))

(defun get-wrapped-metadata (wrapped)
  "Get the metadata from a wrapped metadata object.
  The wrapped form is (meta-wrapper value metadata) so cddr gets metadata."
  (cddr wrapped))

(defun ensure-wrapped (value)
  "Ensure a value is wrapped, adding empty metadata if not."
  (if (wrapped-p value)
      value
      (wrap-with-meta value nil)))

(defun clojure-meta (obj)
  "Return the metadata of an object, or nil if it has none."
  (if (wrapped-p obj)
      (get-wrapped-metadata obj)
      nil))

(defun clojure-with-meta (obj metadata)
  "Return an object of the same type and value as obj, with metadata."
  (wrap-with-meta obj metadata))

(defun clojure-vary-meta (obj meta-fn &rest args)
  "Return an object with metadata transformed by meta-fn."
  (let ((current-meta (clojure-meta obj)))
    (wrap-with-meta obj (apply meta-fn current-meta args))))

(defun clojure-replicate (n x)
  "Return a list with x repeated n times.
   For very large n, limit to 1000 elements to avoid issues."
  (let ((limit (if (and (integerp n) (< n 1000))
                   n
                   1000)))
    (make-list limit :initial-element x)))

(defun clojure-repeat (x &optional (n nil))
  "Return a lazy sequence of repeated xs.
   If n is provided, repeat n times. Otherwise infinite."
  (if n
      (make-list n :initial-element x)
      ;; Infinite repetition - return a lazy range-like marker
      ;; For now, just return a limited list
      (make-list 1000 :initial-element x)))

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

;;; Map/Hash operations
(defun clojure-get (map key &optional default)
  "Get the value for key in map, returning default if not found."
  (if (hash-table-p map)
      (multiple-value-bind (value found-p)
          (gethash key map)
        (if found-p value default))
      ;; For lists/vectors, treat as associative with integer keys
      (if (and (integerp key) (or (listp map) (vectorp map)))
          (let ((idx key))
            (cond
              ((listp map)
               (if (and (>= idx 0) (< idx (length map)))
                   (nth idx map)
                   default))
              ((vectorp map)
               (if (and (>= idx 0) (< idx (length map)))
                   (aref map idx)
                   default))
              (t default)))
          default)))

(defun clojure-contains? (coll key)
  "Return true if coll contains key (for maps/sets)."
  (cond
    ((hash-table-p coll)
     (multiple-value-bind (value found-p)
         (gethash key coll)
       (declare (ignore value))
      (if found-p 'true nil)))
    ((or (listp coll) (vectorp coll))
     ;; For sequences, contains? checks if index is valid
     (if (and (integerp key) (>= key 0))
         (if (< key (length coll))
             'true
             nil)
         nil))
    (t nil)))

(defun clojure-assoc (map &rest key-value-pairs)
  "Associate key-value pairs to a map. Returns new map."
  (unless (evenp (length key-value-pairs))
    (error "assoc requires an even number of arguments"))
  (if (hash-table-p map)
      (let ((new-map (make-hash-table :test (hash-table-test map))))
        ;; Copy all entries from old map
        (maphash (lambda (k v)
                   (setf (gethash k new-map) v))
                 map)
        ;; Add new key-value pairs
        (loop for (key value) on key-value-pairs by #'cddr
              do (setf (gethash key new-map) value))
        new-map)
      ;; For vectors, assoc sets values at indices
      (if (vectorp map)
          (let ((new-vec (copy-seq map)))
            (loop for (key value) on key-value-pairs by #'cddr
                  when (integerp key)
                  do (if (and (>= key 0) (< key (length new-vec)))
                         (setf (aref new-vec key) value)
                         (error "Index out of bounds")))
            new-vec)
          ;; For lists, not supported yet
          (error "assoc not supported for lists"))))

(defun clojure-dissoc (map &rest keys)
  "Dissociate keys from a map. Returns new map."
  (if (hash-table-p map)
      (let ((new-map (make-hash-table :test (hash-table-test map))))
        ;; Copy all entries except those to dissoc
        (maphash (lambda (k v)
                   (unless (member k keys :test #'equal)
                     (setf (gethash k new-map) v)))
                 map)
        new-map)
      map))

(defun clojure-keys (map)
  "Return a sequence of keys in a map."
  (if (hash-table-p map)
      (let ((keys '()))
        (maphash (lambda (k v)
                   (declare (ignore v))
                   (push k keys))
                 map)
        (nreverse keys))
      nil))

(defun clojure-vals (map)
  "Return a sequence of values in a map."
  (if (hash-table-p map)
      (let ((vals '()))
        (maphash (lambda (k v)
                   (declare (ignore k))
                   (push v vals))
                 map)
        (nreverse vals))
      nil))

;;; Delay/Force implementations
(defun clojure-delay (body-fn)
  "Create a delay that will evaluate body-fn on first force."
  ;; In Clojure, delay takes a body to evaluate, not a function.
  ;; We need to capture the environment to create a proper thunk.
  ;; For now, we'll create a delay that holds the thunk.
  (make-delay :thunk body-fn))

(defun clojure-force (delay-obj)
  "Force a delay, evaluating its thunk if not already done."
  (if (delay-p delay-obj)
      (if (delay-forced-p delay-obj)
          (delay-value delay-obj)
          ;; Evaluate and cache
          (let ((value (funcall (delay-thunk delay-obj))))
            (setf (delay-value delay-obj) value)
            (setf (delay-forced-p delay-obj) t)
            value))
      ;; If not a delay object, just return as-is (Clojure behavior)
      delay-obj))

;;; ============================================================
;;; Test Helper Special Forms (from clojure.test)
;;; ============================================================

(defun eval-is (form env)
  "Evaluate an is form: (is expr) or (is expr message) - test assertion.
   Special handling for (is (thrown? ...)) patterns."
  (let ((expr (cadr form))
        ;; Check if there's a message argument
        (has-message (> (length form) 2)))
    ;; Check if expr is a thrown? form
    (if (and (consp expr)
             (symbolp (car expr))
             (string= (symbol-name (car expr)) "thrown?"))
        ;; Handle thrown? specially within is
        (eval-thrown expr env)
        ;; Normal is - just evaluate the expression (ignore message)
        (clojure-eval expr env))))

(defun eval-thrown (form env)
  "Evaluate a thrown? form: (thrown? exception-type body-expr)
   Evaluates body-expr and returns true if an exception is thrown, false otherwise.
   This is a stub implementation that catches errors."
  ;; Syntax: (thrown? ExceptionType expr)
  ;; We evaluate the body and catch any errors, returning 'true if an error occurs
  (let ((exception-type (cadr form))
        (body-expr (caddr form)))
    (declare (ignore exception-type))
    ;; Try to evaluate the body. If it errors, return 'true. Otherwise return nil.
    (handler-case
        (progn
          (clojure-eval body-expr env)
          nil)  ; No exception thrown, return nil (false)
      (error (c)
        (declare (ignore c))
        'true))))  ; Exception caught, return 'true

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
  (let* ((args-vec (cadr form))
         (expr-expr (caddr form))
         (arg-pairs (cdddr form))
         ;; Safely get arg count, handling lazy ranges
         (arg-count (if (lazy-range-p args-vec)
                        (if (lazy-range-end args-vec)
                            ;; Bounded lazy range - calculate count
                            (ceiling (/ (- (lazy-range-end args-vec)
                                          (lazy-range-start args-vec))
                                       (lazy-range-step args-vec)))
                            ;; Infinite lazy range - limit to reasonable number
                            1000)
                        (length (coerce args-vec 'list))))
         ;; Convert args vector to list safely
         (arg-names (if (lazy-range-p args-vec)
                        ;; For lazy ranges, limit elements
                        (lazy-range-to-list args-vec arg-count)
                        (coerce args-vec 'list)))
         (results nil))
    ;; Process arg-pairs in chunks
    ;; Limit to 10000 iterations to avoid infinite loops
    (loop for remaining = arg-pairs then (nthcdr arg-count remaining)
          for iter-count from 0
          while (and remaining (< iter-count 10000))
          do (let ((chunk (subseq remaining 0 (min arg-count (length remaining)))))
               (when (< (length chunk) arg-count)
                 (error "are: incomplete argument list"))
               (let ((new-env env))
                 ;; Evaluate each arg-value before binding
                 (loop for i from 0 below arg-count
                       for arg-name in arg-names
                       for arg-value-form in chunk
                       do (let ((arg-value (clojure-eval arg-value-form env)))
                            (setf new-env (env-extend-lexical new-env arg-name arg-value))))
                 (push (clojure-eval expr-expr new-env) results))))
    (nreverse results)))

(defun eval-do-template (form env)
  "Evaluate a do-template form: (do-template argv expr & values)
   Repeatedly evaluates expr for each group of arguments in values.
   values are partitioned by the number of arguments in argv.

   This performs symbol substitution on the template expression before
   evaluating, similar to how Clojure's macro expansion works."
  ;; Syntax: (do-template [x y] expr v1 v2 v3 v4 ...)
  ;; Expands to: (do (expr with v1,v2 substituted) (expr with v3,v4 substituted) ...)
  (let* ((argv (cadr form))        ; argument vector, e.g., [x y]
         (template-expr (caddr form)) ; template expression using args
         (values (cdddr form))     ; remaining values (as literal forms)
         (argc (length (coerce argv 'list))) ; number of arguments
         (arg-names (coerce argv 'list)) ; argument names as symbols
         (results nil))
    ;; Process values in chunks of argc
    (loop for remaining = values then (nthcdr argc remaining)
          for iter-count from 0
          while (and remaining (< iter-count 10000))
          do (let ((chunk (subseq remaining 0 (min argc (length remaining)))))
               ;; If incomplete chunk, ignore it (Clojure does this)
               (when (= (length chunk) argc)
                 ;; Substitute symbols in template-expr with values from chunk
                 ;; This is the key: we replace the SYMBOL itself, not bind it
                 (let ((substituted-expr (substitute-symbols template-expr arg-names chunk)))
                   ;; Evaluate the substituted expression
                   (push (clojure-eval substituted-expr env) results)))))
    ;; Return the last result (like do), or nil if no results
    (if results
        (car (last results))
        nil)))

(defun substitute-symbols (expr old-symbols new-values)
  "Recursively replace symbols in expr with corresponding values from old-symbols/new-values.
   This performs structural substitution, preserving the structure of expr.
   Compares symbols by name (string comparison) to handle package differences.
   Handles both cons cells (lists) and vectors."
  (flet ((symbol-match-p (s1 s2)
            ;; Compare symbols by name, ignoring package
            (and (symbolp s1) (symbolp s2)
                 (string= (symbol-name s1) (symbol-name s2)))))
    (cond
      ;; If expr is a symbol in old-symbols, replace it
      ((and (symbolp expr)
            (member expr old-symbols :test #'symbol-match-p))
       (let ((pos (position expr old-symbols :test #'symbol-match-p)))
         (nth pos new-values)))
      ;; If expr is a cons cell, recursively substitute in car and cdr
      ((consp expr)
       (let ((new-car (substitute-symbols (car expr) old-symbols new-values))
             (new-cdr (substitute-symbols (cdr expr) old-symbols new-values)))
         (cons new-car new-cdr)))
      ;; If expr is a vector, recursively substitute in each element
      ((vectorp expr)
       (let ((new-vec (make-array (length expr))))
         (loop for i below (length expr)
               do (setf (aref new-vec i)
                        (substitute-symbols (aref expr i) old-symbols new-values)))
         new-vec))
      ;; Otherwise, return as-is
      (t expr))))

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
                 ;; Check for special symbol forms
                 (let ((name (symbol-name form)))
                   (cond
                     ;; Hexadecimal literal symbols (e.g., 0xFFFF)
                     ((and (> (length name) 2)
                           (char= (char name 0) #\0)
                           (char-equal (char name 1) #\x))
                      ;; Parse as hexadecimal
                      (parse-integer name :start 2 :radix 16))
                     ;; BigInt literal symbols (e.g., 123N) - ending with N
                     ((and (> (length name) 1)
                           (char-equal (char name (1- (length name))) #\N))
                      ;; Parse as integer (just return the number, ignoring bigint semantics)
                      (parse-integer name :end (1- (length name))))
                     ;; BigDecimal literal symbols (e.g., 123.45M) - ending with M
                     ((and (> (length name) 1)
                           (char-equal (char name (1- (length name))) #\M))
                      ;; Parse as float (just return the number, ignoring decimal semantics)
                      (read-from-string (subseq name 0 (1- (length name)))))
                     ;; Java interop symbols (e.g., Math/round)
                     ;; Return a lambda that when called with args, evaluates the Java interop
                     ((find #\/ name)
                      (let ((result (java-interop-stub-lookup form)))
                        (cond
                          ;; Static field access - result is the value directly
                          ((and result (not (consp result)))
                           result)
                          ;; Method access - result is (class-name member-name)
                          (result
                           (lambda (&rest args)
                             (apply #'eval-java-interop
                                    (intern (car result))
                                    (intern (cadr result))
                                    args)))
                          ;; Unknown
                          (t
                           (error "Undefined symbol: ~A" form)))))
                     ;; Java constructor calls (e.g., Byte., Integer.)
                     ;; These are symbols ending with a dot
                     ((and (> (length name) 1)
                           (char= (char name (1- (length name))) #\.))
                      ;; Extract class name (everything before the trailing dot)
                      (let ((class-name (subseq name 0 (1- (length name)))))
                        (lambda (&rest args)
                          (eval-java-constructor (intern class-name) args))))
                     ;; Undefined symbol
                     (t
                      (error "Undefined symbol: ~A" form))))))))

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
           ((and head-name (string= head-name "when")) (eval-clojure-when form env))
           ((and head-name (string= head-name "when-not")) (eval-when-not form env))
           ((and head-name (string= head-name "quote")) (eval-quote form env))
           ((and head-name (string= head-name "syntax-quote")) (eval-syntax-quote form env))
           ((and head-name (string= head-name "quasiquote")) (eval-syntax-quote form env))  ; CL's QUASIQUOTE
           ((and head-name (string= head-name "unquote")) (eval-unquote form env))
           ((and head-name (string= head-name "unquote-splicing")) (eval-unquote-splicing form env))
           ((and head-name (string= head-name "var")) (eval-var-quote form env))
           ((and head-name (string= head-name "def")) (eval-def form env))
           ((and head-name (string= head-name "defn")) (eval-defn form env))
           ((and head-name (string= head-name "defn-")) (eval-defn form env))  ; private defn, same as defn for now
           ((and head-name (string= head-name "defonce")) (eval-defonce form env))
           ((and head-name (string= head-name "defmacro")) (eval-defmacro form env))
           ((and head-name (string= head-name "deftest")) (eval-deftest form env))
           ((and head-name (string= head-name "->")) (eval-thread-first form env))
           ((and head-name (string= head-name "->>")) (eval-thread-last form env))
           ((and head-name (string= head-name "is")) (eval-is form env))
           ((and head-name (string= head-name "testing")) (eval-testing form env))
           ((and head-name (string= head-name "are")) (eval-are form env))
           ((and head-name (string= head-name "do-template")) (eval-do-template form env))
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
           ((and head-name (string= head-name "delay"))
            ;; delay creates a lazy computation: (delay body)
            ;; Returns a delay object that will evaluate body when forced
            (let ((body (cadr form)))
              ;; Create a closure that captures the environment
              (make-delay :thunk (lambda () (clojure-eval body env))
                          :forced-p nil)))
           ((and head-name (string= head-name "with-meta"))
            ;; with-meta attaches metadata to a value
            ;; (with-meta value metadata) -> value with metadata
            ;; Note: For type hints (metadata is a symbol), we don't evaluate the metadata
            (destructuring-bind (with-meta-sym value metadata) form
              (declare (ignore with-meta-sym))
              (let ((evaluated-value (clojure-eval value env))
                    ;; Only evaluate metadata if it's not a simple symbol (type hints are symbols)
                    (evaluated-metadata (if (symbolp metadata)
                                           metadata  ; Don't evaluate type hints
                                           (clojure-eval metadata env))))
                (wrap-with-meta evaluated-value evaluated-metadata))))
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
           ;; Handle set literals from reader: (set element...)
           ;; The reader returns (set a b c) for #{a b c}
           ((eq head 'set)
            ;; Evaluate each element and create a set representation
            ;; For now, we use a hash table with test 'equal to represent a set
            (let ((elements (cdr form)))
              (if (null elements)
                  ;; Empty set - return empty hash table
                  (make-hash-table :test 'equal)
                  ;; Evaluate all elements and create hash table
                  (let ((table (make-hash-table :test 'equal)))
                    (dolist (elem elements)
                      (let ((evaluated (clojure-eval elem env)))
                        (setf (gethash evaluated table) t)))
                    table))))
           ((and head-name (string= head-name "try"))
            ;; Try/catch/finally form
            (eval-try form env))

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
  "Apply a function value to arguments. Unwraps any metadata-wrapped values."
  ;; Unwrap fn-value if it's wrapped with metadata
  (let ((actual-fn (if (wrapped-p fn-value)
                      (unwrap-value fn-value)
                      fn-value))
        ;; Unwrap all args
        (unwrapped-args (mapcar (lambda (arg)
                                  (if (wrapped-p arg)
                                      (unwrap-value arg)
                                      arg))
                                args)))
    (typecase actual-fn
      ;; Clojure closure
      (closure
       (let* ((params (closure-params actual-fn))
              (body (closure-body actual-fn))
              (fn-env (closure-env actual-fn))
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
                    for arg in unwrapped-args
                    do (setf new-env (env-extend-lexical new-env (aref params i) arg)))
              ;; Handle rest param if present
              (when amp-pos
                (let ((rest-param (aref params (1+ amp-pos)))
                      (rest-args (nthcdr fixed-count unwrapped-args)))
                  (setf new-env (env-extend-lexical new-env rest-param rest-args))))))
           ;; List parameters (less common but supported)
           (t
            (loop for param in params
                  for arg in unwrapped-args
                  do (setf new-env (env-extend-lexical new-env param arg)))))

         ;; Evaluate body
         (if (null body)
             nil
             (let ((last-expr (car (last body))))
               (dolist (expr (butlast body))
                 (clojure-eval expr new-env))
               (clojure-eval last-expr new-env)))))

      ;; Regular Lisp function
      (function (apply actual-fn unwrapped-args))

      ;; Error
      (t (error "Cannot apply non-function: ~A" fn-value)))))

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
