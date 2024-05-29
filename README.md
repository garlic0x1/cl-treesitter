Common Lisp bindings for libtree-sitter

# High level

The high level API, in the namespace `treesitter` (nicknamed `ts` here),
is garbage collected using finalizers and tries to make treesitter "lispier".

```lisp
;; load a language grammar
(cffi:use-foreign-library "libtree-sitter-c.so")
(cffi:defcfun "tree_sitter_c" :pointer)
(defvar *c-lang* (tree-sitter-c))

;; parse to stringified node
(let ((parser (ts:make-parser :language *c-lang*)))
  (ts:node-string
   (ts:tree-root-node
    (ts:parser-parse-string parser "1+1;"))))
;;=> "(translation_unit (expression_statement (update_expression argument: (binary_expression left: (number_literal) right: (number_literal)) operator: (MISSING \"--\"))))"

;; make a query
(line-up-first
 (ts:make-parser :language *c-lang*)
 (ts:parser-parse-string "int func() { return 0; return 1; }")
 (ts:tree-root-node)
 (ts:query "(return_statement) @x"))
;;=> (#<TS:NODE {10050627D3}> #<TS:NODE {1005062713}>)
```

# Low level

The namespace `treesitter/bindings` is a thin wrapper around the C library.
Everything in the bindings package is prefixed with `ts-*`, and objects created
must be manually freed with the `ts-*-delete` functions.

```lisp
(in-package :treesitter/bindings)

(let* ((parser (ts-parser-new :language *c-lang*))
       (tree (ts-parser-parse-string parser "1+1;"))
       (root (ts-tree-root-node tree)))
  (print (ts-node-string root))
  (ts-node-delete root)
  (ts-tree-delete tree)
  (ts-parser-delete parser))
```
