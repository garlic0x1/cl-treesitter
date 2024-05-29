Common Lisp bindings for libtree-sitter

There are two APIs, the package `:treesitter/bindings` has low-level C bindings
with manual memory management, you need to clean up any resources you create with
the `ts-*-delete` functions.

The package `:treesitter` provides a garbage collected API using finalizers, and
it has a "lispier" interface.

# Example

Here is the high-level API:

```lisp
(cffi:use-foreign-library "libtree-sitter-c.so")
(cffi:defcfun "tree_sitter_c" :pointer)
(defvar *c-lang* (tree-sitter-c))

(let ((parser (ts:make-parser :language *c-lang*)))
  (ts:node-string
   (ts:tree-root-node
    (ts:parser-parse-string parser "1+1;"))))
```

Here is the low-level API:

```lisp
(cffi:use-foreign-library "libtree-sitter-c.so")
(cffi:defcfun "tree_sitter_c" :pointer)
(defvar *c-lang* (tree-sitter-c))

(let* ((parser (ts-parser-new :language *c-lang*))
       (tree (ts-parser-parse-string parser "1+1;"))
       (root (ts-tree-root-node tree)))
  (print (ts-node-string root))
  (ts-node-delete root)
  (ts-tree-delete tree)
  (ts-parser-delete parser))
```

```lisp
"(translation_unit (expression_statement (update_expression argument: (binary_expression left: (number_literal) right: (number_literal)) operator: (MISSING \"--\"))))" ; No value
```
