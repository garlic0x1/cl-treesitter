(defpackage :treesitter/test/bindings
  (:use :cl :fiveam :treesitter/bindings))
(in-package :treesitter/test/bindings)

(def-suite :treesitter/bindings
  :description "Tests for treesitter low-level bindings.")
(in-suite :treesitter/bindings)

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
