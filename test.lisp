(in-package :treesitter)

(ts-use-library "c")

(with-ts-parser (parser "c")
  (with-ts-tree (tree parser "1+1;")
    (with-ts-tree-root-node (node tree)
      (print (ts-node-string node)))))
