(in-package :treesitter)

;; load the C language grammar
(use-foreign-library "libtree-sitter-c.so")
(defcfun "tree_sitter_c" :pointer)

(let* ((parser (ts-parser-new))
       (language (tree-sitter-c)))
  (ts-parser-set-language parser language)
  (let* ((parsed (ts-parser-parse-string parser (cffi:null-pointer) "1+1;" 4))
         (root (ts-tree-root-node parsed)))
    (print root)
    (ts-node-type (convert-to-foreign root))))
