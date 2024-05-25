(defpackage :treesitter/test
  (:use :cl :fiveam :treesitter))
(in-package :treesitter/test)

(def-suite :treesitter
  :description "Tests for treesitter")
(in-suite :treesitter)

(cffi:use-foreign-library "libtree-sitter-c.so")
(cffi:defcfun "tree_sitter_c" :pointer)

(test :basic
  "Ensure parsing strings works."
  (let ((lang (tree-sitter-c))
        (parser (ts-parser-new)))
    (ts-parser-set-language parser lang)
    (let* ((tree (ts-parser-parse-string parser (cffi:null-pointer) "1+1;" 4))
           (root (ts-tree-root-node tree)))
      (is (ts-node-string root))
      (ts-node-delete root)
      (ts-tree-delete tree)
      (ts-language-delete lang)
      (ts-parser-delete parser))))
