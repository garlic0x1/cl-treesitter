Common Lisp bindings for libtree-sitter

These are low-level bindings, you need to clean up resources with the `ts-*-delete` functions after creating them.

# Example

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
