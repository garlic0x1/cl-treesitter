Common Lisp bindings for libtree-sitter

These are low-level bindings, you need to clean up resources with the `ts-*-delete` functions after creating them.

# Example

```lisp
(cffi:use-foreign-library "libtree-sitter-c.so")
(cffi:defcfun "tree_sitter_c" :pointer)

(let ((lang (tree-sitter-c))
     (parser (ts-parser-new)))
 (ts-parser-set-language parser lang)
 (let* ((tree (ts-parser-parse-string parser (cffi:null-pointer) "1+1;" 4))
        (root (ts-tree-root-node tree)))
   (is (ts-node-string root))
   (ts-node-delete root)
   (ts-tree-delete tree)
   (ts-language-delete lang)
   (ts-parser-delete parser)))
```
