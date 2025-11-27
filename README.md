# Common Lisp Clojure Syntax

A Common Lisp library that adds Clojure-like syntax support, enabling the use of vector (`[]`) and map (`{}`) literals in Common Lisp code.

## Overview

This library extends the Common Lisp reader to support Clojure's concise syntax for collections:

- **Vector literals** `[...]` → Common Lisp vectors
- **Map literals** `{...}` → Common Lisp hash tables

## Installation

Load the system using ASDF:

```lisp
(asdf:load-system :cl-clojure-syntax)
```

## Usage

### Enabling Clojure Syntax

```lisp
(cl-clojure-syntax:enable-clojure-syntax)
```

Once enabled, you can use Clojure-style syntax in your code:

```lisp
;; Vector literals
[1 2 3 4 5]
["a" "b" "c"]

;; Map literals
{:name "John" :age 30}
{"key1" "value1" "key2" "value2"}
```

### Disabling Clojure Syntax

To revert to standard Common Lisp syntax:

```lisp
(cl-clojure-syntax:disable-clojure-syntax)
```

## Implementation Details

### Vector Literals (`[]`)

Vector literals `[...]` are converted to Common Lisp vectors using the `VECTOR` function.

```lisp
[1 2 3] → #(1 2 3)
```

### Map Literals (`{}`)

Map literals `{...}` are converted to Common Lisp hash tables with `:test 'equal` for flexible key matching. The contents must have an even number of elements (alternating keys and values).

```lisp
{:a 1 :b 2} → #<HASH-TABLE :test EQUAL>
```

If an odd number of elements is provided, an error is raised.

## Testing

Run the test suite:

```lisp
(asdf:test-system :cl-clojure-syntax)
```

## License

MIT

## Author

Droid
