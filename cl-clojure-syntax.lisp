(in-package #:cl-clojure-syntax)

;;; Clojure uses a specialized reader that preserves case.
;;; We create a separate readtable for Clojure code instead of modifying
;;; the global one, since :preserve mode breaks Common Lisp's t/nil.

(defvar *clojure-readtable* nil
  "The readtable used for reading Clojure code.")

(defun read-tagged-literal (stream sub-char num)
  "Read a Clojure tagged literal: #tag value
   Examples: #uuid \"...\", #inst \"...\", #RecordType[field1 field2]
   Returns (tagged-literal tag value)."
  (declare (ignore num))
  ;; The dispatch character (letter) has been consumed but we need to read the full tag
  ;; Put the letter back and read the symbol
  (unread-char sub-char stream)
  ;; Read the tag symbol (everything until whitespace or special char)
  (let ((tag-chars (list)))
    ;; Read characters that form the tag
    (loop
      (let ((c (peek-char nil stream nil nil)))
        (cond
          ((null c) (return))
          ;; Tag ends at whitespace or special characters (but not . which is part of namespaced tags)
          ((member c '(#\Space #\Tab #\Newline #\Return #\( #\) #\[ #\] #\{ #\} #\" #\; #\,) :test #'char=)
           (return))
          (t
           (push (read-char stream) tag-chars)))))
    (let ((tag-name (coerce (nreverse tag-chars) 'string)))
      ;; Read the value that follows the tag
      (let ((value (read stream t nil t)))
        ;; Return tagged literal representation
        (list 'tagged-literal (intern tag-name) value)))))

(defun read-character-literal (stream char)
  "Read a Clojure character literal: \\a, \\newline, etc.
   Returns a Common Lisp character object.
   Supports: \\a (single char), \\newline, \\space, \\tab, \\return, \\backspace, \\formfeed"
  (declare (ignore char))
  ;; Read the next character(s)
  (let ((first-char (read-char stream t nil)))
    (cond
      ;; Named characters
      ((and (char= first-char #\n)
            (peek-char t stream nil nil)
            (char= (peek-char t stream nil nil) #\e))
       ;; Check for \newline
       (let ((name (make-array 0 :element-type 'character :fill-pointer 0)))
         (vector-push-extend first-char name)
         (loop while (and (peek-char t stream nil nil)
                         (alpha-char-p (peek-char t stream nil nil)))
               do (vector-push-extend (read-char stream t nil) name))
         (let ((name-str (string-downcase name)))
           (cond
             ((string= name-str "newline") #\Newline)
             ((string= name-str "space") #\Space)
             ((string= name-str "tab") #\Tab)
             ((string= name-str "return") #\Return)
             ((string= name-str "backspace") #\Backspace)
             ((string= name-str "formfeed") #\Page)
             ((string= name-str "u")
              ;; Unicode escape \uXXXX - not fully implemented, return placeholder
              (dotimes (i 4) (read-char stream t nil))
              #\?)
             (t
              ;; Unknown named character - return the first char as-is
              (char name 0))))))
      ;; Single character
      (t first-char))))

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

    ;; , - comma for unquote (not whitespace in Clojure's quasiquote context)
    ;; Note: In regular code comma is whitespace, but in backquote context it's unquote
    ;; We set it to our unquote handler to support syntax-quote
    (set-macro-character #\, #'read-clojure-unquote nil *clojure-readtable*)

    ;; \char - character literal (Clojure syntax)
    ;; In Clojure, \a is a character literal. In CL, character literals are #\a
    (set-macro-character #\\ #'read-character-literal nil *clojure-readtable*)

    ;; ::foo - auto-resolved keyword
    (set-macro-character #\: #'read-colon-dispatch nil *clojure-readtable*)

    ;; #"regex" - regex literal
    (set-dispatch-macro-character #\# #\" #'read-regex *clojure-readtable*)

    ;; #{...} - set literal
    (set-dispatch-macro-character #\# #\{ #'read-set *clojure-readtable*)

    ;; #_form - comment (skip next form)
    (set-dispatch-macro-character #\# #\_ #'read-comment *clojure-readtable*)

    ;; #^metadata form - metadata (deprecated, same as ^metadata)
    (set-dispatch-macro-character #\# #\^ (lambda (s c n)
                                              (declare (ignore c n))
                                              (funcall (symbol-function 'read-metadata) s #\^))
                            *clojure-readtable*)

    ;; ^metadata form - metadata syntax
    (set-macro-character #\^ (lambda (s c)
                               (declare (ignore c))
                               (funcall (symbol-function 'read-metadata) s #\^))
                        nil *clojure-readtable*)

    ;; ##Inf, ##-Inf, ##NaN - special float literals
    (set-dispatch-macro-character #\# #\# #'read-special-float *clojure-readtable*)

    ;; 'form - quote (non-terminating so ss' is read as one symbol in Clojure)
    (set-macro-character #\' #'read-quote t *clojure-readtable*)

    ;; #'form - var quote
    (set-dispatch-macro-character #\# #\' #'read-var-quote *clojure-readtable*)

    ;; `form - syntax-quote
    ;; We need to set this to non-terminating T to override CL's quasiquote
    (set-macro-character #\` #'read-syntax-quote t *clojure-readtable*)

    ;; ~form - unquote (and ~@form for unquote-splicing)
    (set-macro-character #\~ #'read-tilde-dispatch *clojure-readtable*)

    ;; #(body) - anonymous function literal
    (set-dispatch-macro-character #\# #\( #'read-anon-fn *clojure-readtable*)

    ;; Tagged literals: #tag value (like #uuid "...", #inst "...", #RecordType[...])
    ;; Override CL's default dispatch chars for letters to handle Clojure tagged literals
    (dolist (char '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
                    #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
                    #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
                    #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))
      (set-dispatch-macro-character #\# char #'read-tagged-literal *clojure-readtable*)))
  *clojure-readtable*)

(defun read-vector (stream char)
  (declare (ignore char))
  (apply #'vector (read-delimited-list #\] stream t)))

(defun read-whitespace-char (stream char)
  "Read a comma as whitespace in Clojure.
   In Clojure, commas are treated as whitespace characters.
   This function reads and discards the comma, then reads the next form."
  (declare (ignore char))
  ;; Read the next form (comma is just whitespace)
  (read stream t nil t))

(defun read-map (stream char)
  (declare (ignore char))
  (let ((items (read-delimited-list #\} stream t))
        (table (make-hash-table :test 'equal)))
    (unless (evenp (length items))
      (error "Map literal must have an even number of elements"))
    (loop for (key value) on items by #'cddr
          do (setf (gethash key table) value))
    table))

(defun read-set (stream sub-char num)
  "Read a Clojure set literal: #{1 2 3}
   Returns a list (set elements...) to represent the set.
   In a full implementation, this would create an actual set data structure."
  (declare (ignore sub-char num))
  (let ((items (read-delimited-list #\} stream t)))
    ;; Return a set representation as a list
    ;; For now, we use (set items...) as a marker
    ;; TODO: In a full implementation, this would create a hash set
    ;; Check for duplicates (Clojure sets don't allow duplicates)
    (let ((unique-items (remove-duplicates items :test 'equal)))
      (when (< (length unique-items) (length items))
        ;; Warn about duplicates but don't error
        (warn "Duplicate elements in set literal"))
      (list* 'set unique-items))))

(defvar *comment-marker* (make-symbol "COMMENT-MARKER")
  "Special marker used to indicate a commented form that should be filtered out.")

(defun read-comment (stream sub-char num)
  "Read a Clojure comment: #_form
   This reads and discards the next form.
   Uses read-suppress to skip the form, then reads the next form."
  (declare (ignore sub-char num))
  ;; Skip the next form using read-suppress
  (let ((*read-suppress* t))
    (read stream t nil t))
  ;; Peek at the next character to see if we can read another form
  (let ((next-char (peek-char t stream nil :eof)))
    (cond
      ((eq next-char :eof)
       ;; End of file
       :eof)
      ((member next-char '(#\] #\} #\)) :test #'char=)
       ;; At a delimiter - this means #_ was at the end of a list
       ;; Return a special marker that should be filtered out
       *comment-marker*)
      (t
       ;; There's another form, read it
       (read stream t nil t)))))

(defun read-metadata (stream char)
  "Read Clojure metadata syntax: ^metadata-form
   This reads metadata and attaches it to the following form.
   Returns a list (with-meta form metadata) to represent the metadata."
  (declare (ignore char))
  ;; Read the metadata (can be a keyword, symbol, or map)
  (let ((metadata (read stream t nil t)))
    ;; Read the form to attach metadata to
    (let ((form (read stream t nil t)))
      ;; Return a representation with metadata
      ;; For now, we use (with-meta form metadata)
      (list 'with-meta form metadata))))

(defun read-metadata-dispatch (stream sub-char num)
  "Read Clojure metadata syntax: #^metadata-form (deprecated, same as ^)
   This is the dispatch version of metadata syntax."
  (declare (ignore sub-char num))
  ;; Delegate to the regular metadata reader
  (read-metadata stream #\^))

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

(defun read-regex (stream sub-char num)
  "Read a Clojure regex literal: #\"pattern\"
   Returns a list (regex pattern) to represent the regex.
   In a full implementation, this would compile to a regex object."
  (declare (ignore sub-char num))
  ;; After #\" is consumed, we need to read the string content manually
  ;; since the opening quote has already been consumed by the dispatch reader
  (let ((chars '()))
    ;; Read characters until we find an unescaped closing quote
    (loop
      (let ((char (read-char stream t nil)))
        (cond
          ;; Escaped character - read and add the next char literally
          ((eql char #\\)
           (push #\\ chars)
           (push (read-char stream t nil) chars))
          ;; Closing quote - we're done
          ((eql char #\")
           (return))
          ;; Regular character - add to list
          (t
           (push char chars)))))
    ;; Reverse and convert to string
    (let ((pattern (coerce (nreverse chars) 'string)))
      ;; Return a regex representation as a list
      ;; For now, we use (regex pattern) as a marker
      ;; TODO: In a full implementation, this would compile to a regex object
      (list 'regex pattern))))

(defun read-special-float (stream sub-char num)
  "Read Clojure special float literals: ##Inf, ##-Inf, ##NaN
   These represent positive infinity, negative infinity, and NaN respectively."
  (declare (ignore sub-char num))
  ;; Read the token after ##
  (let ((token (read stream t nil t)))
    (cond
      ;; Check for symbol or string that matches special floats
      ((or (symbolp token) (stringp token))
       (let ((name (if (symbolp token) (symbol-name token) token)))
         (cond
           ((string= name "Inf") sb-ext:single-float-positive-infinity)
           ((string= name "-Inf") sb-ext:single-float-negative-infinity)
           ((string= name "NaN")
            ;; Create NaN using IEEE 754 quiet NaN bit pattern for single float
            ;; Bit pattern: 0x7FC00000 (sign=0, exp=255, mantissa with high bit set)
            (sb-kernel:make-single-float #x7FC00000))
           (t (error "Unknown special float literal: ##~A" name)))))
      ;; Check for number (in case someone writes ##1.0)
      ((numberp token)
       (if (or (sb-ext:float-infinity-p token) (sb-ext:float-nan-p token))
           token
           (error "Not a special float value: ~A" token)))
      (t (error "Invalid special float syntax: expected Inf, -Inf, or NaN, got ~A" token)))))

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

(defun parse-suffixed-number (symbol)
  "Parse a symbol that might be a Clojure suffixed number literal.
   Handles N suffix (bigint) and M suffix (decimal).
   Returns the parsed number, or the original form if not a suffixed number."
  (if (not (symbolp symbol))
      ;; Return non-symbols unchanged (e.g., lists, vectors, numbers)
      symbol
      (let ((name (symbol-name symbol)))
        ;; Check for N suffix (bigint)
        ;; Pattern: digits followed by N
        (when (and (> (length name) 1)
                   (char= (char name (1- (length name))) #\N)
                   (every #'(lambda (c) (or (digit-char-p c) (char= c #\-)))
                         (subseq name 0 (1- (length name)))))
          ;; Parse as integer
          (let ((num-str (subseq name 0 (1- (length name)))))
            (return-from parse-suffixed-number
              (parse-integer num-str))))
        ;; Check for M suffix (decimal)
        ;; Pattern: digits.digits followed by M
        (when (and (> (length name) 2)
                   (char= (char name (1- (length name))) #\M)
                   (find #\. name))
          ;; Parse as float (Common Lisp doesn't have BigDecimal, so we use float)
          (let ((num-str (subseq name 0 (1- (length name)))))
            (return-from parse-suffixed-number
              (read-from-string num-str))))
        ;; Not a suffixed number
        symbol)))

;;; Quote, syntax-quote, and unquote readers

(defun read-quote (stream char)
  "Read a Clojure quote: 'form
   Returns (quote form)."
  (declare (ignore char))
  (list 'quote (read stream t nil t)))

(defun read-var-quote (stream sub-char num)
  "Read a Clojure var quote: #'form
   Returns (var form)."
  (declare (ignore sub-char num))
  (list 'var (read stream t nil t)))

(defun read-syntax-quote (stream char)
  "Read a Clojure syntax quote: `form
   Returns (syntax-quote form).
   In a full implementation, this would resolve symbols and handle unquote."
  (declare (ignore char))
  ;; We use our custom backquote reader, but need to set comma handler too
  (list 'syntax-quote (read stream t nil t)))

(defun read-clojure-unquote (stream char)
  "Read Clojure unquote: ,form or ,@form
   Returns (unquote form) or (unquote-splicing form)."
  (declare (ignore char))
  (let ((next-char (peek-char nil stream nil nil)))
    (if (eql next-char #\@)
        ;; Consume the @ and read the form for unquote-splicing
        (progn
          (read-char stream)  ; consume the @
          (list 'unquote-splicing (read stream t nil t)))
        ;; Regular unquote
        (list 'unquote (read stream t nil t)))))

(defun read-tilde-dispatch (stream char)
  "Dispatch for ~ character. If followed by @, it's unquote-splicing. Otherwise unquote."
  (declare (ignore char))
  (let ((next-char (peek-char nil stream nil nil)))
    (if (eql next-char #\@)
        ;; Consume the @ and read the form for unquote-splicing
        (progn
          (read-char stream)  ; consume the @
          (list 'unquote-splicing (read stream t nil t)))
        ;; Regular unquote
        (list 'unquote (read stream t nil t)))))

;;; Anonymous function reader

(defun read-anon-fn (stream sub-char num)
  "Read a Clojure anonymous function literal: #(body)
   Converts to (fn* [args] body) where args are inferred from % placeholders.
   % or %1 = first arg, %2 = second arg, etc. %& = rest args."
  (declare (ignore sub-char num))
  ;; Read the body forms until closing )
  (let ((body-forms (read-delimited-list #\) stream t)))
    ;; Analyze body to find % placeholders
    (multiple-value-bind (arg-count has-rest)
        (analyze-arg-placeholders body-forms)
      ;; Generate argument names: gen__#, gen__2#, gen__3#, etc.
      (let* ((args (generate-arg-names arg-count has-rest))
             ;; Build arg map for substitution
             (arg-map (build-arg-map arg-count has-rest))
             ;; Replace % placeholders with generated arg names
             (processed-body (mapcar #'(lambda (form) (replace-placeholders form arg-map))
                                    body-forms)))
        ;; Build the fn* form
        `(fn* ,args ,@processed-body)))))

(defun analyze-arg-placeholders (forms)
  "Analyze forms to determine how many arguments are needed and if rest args are used.
   Returns (values arg-count has-rest). Throws error for invalid placeholders."
  (let ((max-arg 0)
        (rest-p nil))
    (labels ((analyze (form)
               (cond
                 ;; Symbol - check for % placeholders
                 ((symbolp form)
                  (let ((name (symbol-name form)))
                    (when (and (> (length name) 0)
                               (char= (char name 0) #\%))
                      ;; Validate and process placeholder
                      (cond
                        ;; %& - rest arguments
                        ((string= name "%&")
                         (setf rest-p t))
                        ;; %% - invalid (double percent)
                        ((string= name "%%")
                         (error "Invalid placeholder: %%"))
                        ;; %%1 - invalid
                        ((and (> (length name) 1) (string= name "%%1"))
                         (error "Invalid placeholder: %%1"))
                        ;; %%2 - invalid
                        ((and (> (length name) 1) (string= name "%%2"))
                         (error "Invalid placeholder: %%2"))
                        ;; %N where N is a number
                        ((and (> (length name) 1)
                              (every #'digit-char-p (subseq name 1)))
                         (let ((arg-num (parse-integer (subseq name 1))))
                           (setf max-arg (max max-arg arg-num))))
                        ;; % or %1 - first argument
                        ((or (string= name "%") (string= name "%1"))
                         (setf max-arg (max max-arg 1)))
                        ;; Check for invalid formats
                        ((and (> (length name) 2)
                              (char= (char name 1) #\/))
                         ;; %1/2 format - invalid
                         (error "Invalid placeholder: ~A" name))
                        ((and (> (length name) 2)
                              (find #\. (subseq name 1)))
                         ;; %1.5 format - invalid
                         (error "Invalid placeholder: ~A" name))
                        ((and (> (length name) 2)
                              (find #\- (subseq name 1)))
                         ;; %-0.2 format - invalid
                         (error "Invalid placeholder: ~A" name))
                        ((and (> (length name) 2)
                              (char= (char name (1- (length name))) #\M))
                         ;; %3M format - invalid
                         (error "Invalid placeholder: ~A" name))
                        ;; Unknown % form
                        (t
                         (error "Invalid placeholder: ~A" name))))))
                 ;; Cons cell - recurse into car and cdr
                 ((consp form)
                  (analyze (car form))
                  (analyze (cdr form)))
                 ;; Vector - recurse into elements
                 ((vectorp form)
                  (dolist (item (coerce form 'list))
                    (analyze item)))
                 ;; Ignore other types
                 (t nil))))
      (dolist (form forms)
        (analyze form))
      (values max-arg rest-p))))

(defun generate-arg-names (arg-count has-rest)
  "Generate argument name list based on count and rest flag.
   Returns: [] for no args, [gen__#] for one arg, etc."
  (if (and (= arg-count 0) (not has-rest))
      '()
      (let ((args '()))
        ;; Generate numbered args
        (loop for i from 1 to arg-count
              do (push (generate-gensym i) args))
        ;; Add rest arg if needed
        (when has-rest
          (push '& args)
          (push (generate-gensym (1+ arg-count)) args))
        ;; Return as vector (for consistency with Clojure's vector syntax)
        (coerce (nreverse args) 'vector))))

(defun generate-gensym (num)
  "Generate a gensym'd symbol with the pattern gen__N#."
  (let ((name (format nil "gen__~D#" num)))
    (make-symbol name)))

(defun build-arg-map (arg-count has-rest)
  "Build a mapping from % placeholders to generated argument symbols.
   Returns an alist mapping % -> gen__#, %1 -> gen__#, %2 -> gen__2#, etc."
  (let ((map '()))
    ;; Map % and %1 to first arg
    (when (> arg-count 0)
      (push (cons "%1" (generate-gensym 1)) map)
      (push (cons "%" (generate-gensym 1)) map))
    ;; Map %2, %3, etc.
    (loop for i from 2 to arg-count
          do (push (cons (format nil "%~D" i) (generate-gensym i)) map))
    ;; Map %& to rest arg
    (when has-rest
      (push (cons "%&" (generate-gensym (1+ arg-count))) map))
    map))

(defun replace-placeholders (form arg-map)
  "Replace % placeholders in form with generated argument symbols from arg-map."
  (typecase form
    (symbol
     (let ((name (symbol-name form)))
       (if (and (> (length name) 0)
                (char= (char name 0) #\%))
           ;; Check if this is a placeholder we should replace
           (let ((replacement (assoc name arg-map :test #'string=)))
             (if replacement
                 (cdr replacement)
                 ;; Not a valid placeholder - return as-is
                 form))
           ;; Not a placeholder - return original symbol
           form)))
    (list
     (mapcar #'(lambda (item) (replace-placeholders item arg-map)) form))
    (vector
     (coerce (mapcar #'(lambda (item) (replace-placeholders item arg-map))
                     (coerce form 'list))
             'vector))
    ;; Return other types as-is
    (t form)))

(defun enable-clojure-syntax ()
  "Enable Clojure syntax for subsequent read operations."
  (ensure-clojure-readtable)
  (setq *readtable* *clojure-readtable*))

;;; Preprocessing for dot-only tokens
;;; Common Lisp's reader rejects tokens consisting only of dots (like "..")
;;; but Clojure uses these as valid symbols. We preprocess the input to
;;; escape such tokens with vertical bars: .. -> |..|

(defun preprocess-clojure-dots (input-string)
  "Preprocess a Clojure source string to escape dot-only tokens.
   Converts tokens like '..' to '|..|' so CL's reader accepts them.
   Also handles standalone '.' which is used for Java interop."
  (with-output-to-string (out)
    (let ((len (length input-string))
          (i 0))
      (labels ((delimiter-p (c)
                 (member c '(#\Space #\Tab #\Newline #\Return #\( #\) #\[ #\] #\{ #\} #\" #\; #\, #\')))
               (at-token-boundary-p (pos)
                 (or (= pos 0)
                     (delimiter-p (char input-string (1- pos)))))
               (ends-token-p (pos)
                 (or (>= pos len)
                     (delimiter-p (char input-string pos)))))
        (loop while (< i len) do
          (let ((c (char input-string i)))
            (cond
              ;; Skip string literals entirely
              ((char= c #\")
               (write-char c out)
               (incf i)
               (loop while (< i len) do
                 (let ((sc (char input-string i)))
                   (write-char sc out)
                   (incf i)
                   (cond
                     ((char= sc #\\)
                      ;; Escape - write next char too
                      (when (< i len)
                        (write-char (char input-string i) out)
                        (incf i)))
                     ((char= sc #\")
                      (return))))))
              ;; Skip comments entirely
              ((char= c #\;)
               (loop while (and (< i len) (char/= (char input-string i) #\Newline)) do
                 (write-char (char input-string i) out)
                 (incf i)))
              ;; Check for dot at token boundary
              ((and (char= c #\.)
                    (at-token-boundary-p i))
               ;; Count consecutive dots
               (let ((dot-start i)
                     (dot-count 0))
                 (loop while (and (< i len) (char= (char input-string i) #\.)) do
                   (incf dot-count)
                   (incf i))
                 ;; Check if this is a dot-only token
                 (if (ends-token-p i)
                     ;; Dot-only token - escape it
                     (progn
                       (write-char #\| out)
                       (loop repeat dot-count do (write-char #\. out))
                       (write-char #\| out))
                     ;; Dots followed by more chars (like .methodName) - write normally
                     (progn
                       (loop repeat dot-count do (write-char #\. out))))))
              ;; Escape | character (CL uses it for escaped symbols)
              ((char= c #\|)
               (write-char #\\ out)
               (write-char c out)
               (incf i))
              ;; Regular character
              (t
               (write-char c out)
               (incf i)))))))))

(defun make-preprocessed-stream (input-string)
  "Create a stream from preprocessed Clojure source."
  (make-string-input-stream (preprocess-clojure-dots input-string)))

(defun read-clojure (stream &optional (eof-error-p t) (eof-value :eof))
  "Read a Clojure form from STREAM, post-processing to handle number suffixes.
   This wraps the standard READ function to handle Clojure-specific syntax like
   the N suffix for bigints and M suffix for decimals."
  (let* ((form (read stream eof-error-p eof-value))
         (processed (if (eq form :eof)
                       :eof
                       (parse-suffixed-number form))))
    processed))

;;; Convert CL's quasiquote to Clojure's syntax-quote format
(defun convert-cl-quasiquote (form)
  "Convert Common Lisp's quasiquote forms to Clojure's syntax-quote format.
   CL uses QUASIQUOTE with internal COMMA structures. We convert these to
   syntax-quote, unquote, and unquote-splicing."
  (typecase form
    ;; Handle SBCL's COMMA structure (for unquote and unquote-splicing)
    ;; SB-IMPL::COMMA has :EXPR and :KIND (0 = unquote, 1 = unquote-splicing)
    ((satisfies sb-impl::comma-p)
     (let ((kind (sb-impl::comma-kind form))
           (expr (sb-impl::comma-expr form)))
       (if (= kind 1)
           `(unquote-splicing ,expr)
           `(unquote ,expr))))
    ;; Handle strings - must come before vector since strings are vectors in CL
    (string form)
    ;; Handle QUASIQUOTE
    (cons
     (if (eq (car form) 'quasiquote)
         ;; Convert quasiquote contents to our format
         `(syntax-quote ,(mapcar #'convert-cl-quasiquote (cdr form)))
         ;; Recursively process list elements
         (mapcar #'convert-cl-quasiquote form)))
    ;; Handle vectors (but not strings, which are handled above)
    (vector
     (coerce (mapcar #'convert-cl-quasiquote (coerce form 'list)) 'vector))
    ;; Everything else is self-evaluating
    (t form)))

(defun read-clojure-string (string &optional (eof-error-p t) (eof-value :eof))
  "Read a Clojure form from a string, with preprocessing for dot tokens.
   This is the main entry point for reading Clojure source code."
  (let ((preprocessed (preprocess-clojure-dots string)))
    (with-input-from-string (stream preprocessed)
      (let ((*readtable* (ensure-clojure-readtable)))
        (convert-cl-quasiquote (read-clojure stream eof-error-p eof-value))))))

(defun disable-clojure-syntax ()
  "Disable Clojure syntax, restoring default Common Lisp readtable."
  ;; Restore default readtable (SBCL's standard readtable)
  (setq *readtable* (copy-readtable nil)))

