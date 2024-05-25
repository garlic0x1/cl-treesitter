(defpackage :treesitter/test
  (:use :cl :fiveam :treesitter))
(in-package :treesitter/test)

(def-suite :treesitter
  :description "Tests for treesitter")
(in-suite :treesitter)

(cffi:use-foreign-library "libtree-sitter-c.so")
(cffi:defcfun "tree_sitter_c" :pointer)
(defvar *c-lang* (tree-sitter-c))

(test :basic
  "Ensure parsing strings works."
  (let* ((parser (ts-parser-new :language *c-lang*))
         (tree (ts-parser-parse-string parser "1+1;"))
         (root (ts-tree-root-node tree)))
    (is (ts-node-string root))
    (ts-node-delete root)
    (ts-tree-delete tree)
    (ts-parser-delete parser)))
