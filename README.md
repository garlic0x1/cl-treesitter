Common Lisp bindings for libtree-sitter

# Installation

You will have to run `make` to build the shim.

```bash
git clone https://github.com/garlic0x1/cl-treesitter
cd cl-treesitter
make
```

# Memory management

Allocations are hidden behind `with-*` macros, so you don't have to worry about it unless you use internal functions.

# Example

```lisp
;; Load the C parser
(ts-use-library "c")

;; Create a parser, tree, and node, then print that node
(with-ts-parser (parser "c")
  (with-ts-tree (tree parser "1+1;")
    (with-ts-tree-root-node (node tree)
      (print (ts-node-string node)))))
```
