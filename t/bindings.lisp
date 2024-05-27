(defpackage :treesitter/test/bindings
  (:use :cl :cffi :fiveam :treesitter/bindings))
(in-package :treesitter/test/bindings)

(def-suite :treesitter/bindings
  :description "Tests for treesitter low-level bindings.")
(in-suite :treesitter/bindings)

(cffi:use-foreign-library "libtree-sitter-c.so")
(cffi:defcfun "tree_sitter_c" :pointer)
(defvar *c-lang* (tree-sitter-c))

(test :basic
  "Ensure parsing strings works."
  ;; allocs
  (let* ((parser (ts-parser-new :language *c-lang*))
         (tree (ts-parser-parse-string parser "1 + 1;"))
         (root (ts-tree-root-node tree)))
    (is (equal
         "(translation_unit (expression_statement (update_expression argument: (binary_expression left: (number_literal) right: (number_literal)) operator: (MISSING \"--\"))))"
         (ts-node-string root)))
    ;; cleanup
    (ts-node-delete root)
    (ts-tree-delete tree)
    (ts-parser-delete parser)))

(test :cursor
  "Test creating and moving a cursor."
  ;; allocs
  (let* ((parser (ts-parser-new :language *c-lang*))
         (tree (ts-parser-parse-string parser "bool test_cursors(539);"))
         (root (ts-tree-root-node tree))
         (cursor (ts-tree-cursor-new root)))
    (ts-tree-cursor-goto-first-child cursor)
    (ts-tree-cursor-goto-first-child cursor)
    (is (equal "type" (ts-tree-cursor-current-field-name cursor)))
    ;; alloc node
    (let ((n (ts-tree-cursor-current-node cursor)))
      (is (equal "primitive_type"
                 (ts-language-symbol-name *c-lang* (ts-node-symbol n))))
      ;; cleanup
      (ts-node-delete n))
    ;; cleanup
    (ts-tree-cursor-delete cursor)
    (ts-node-delete root)
    (ts-tree-delete tree)
    (ts-parser-delete parser)))

(test :query
  (let* ((parser (ts-parser-new :language *c-lang*))
         (source "int main() { return 0; }")
         (query-string "(return_statement) @param_expression")
         (tree (ts-parser-parse-string parser source))
         (root (ts-tree-root-node tree))
         (query (multiple-value-list (ts-query-new *c-lang* query-string)))
         (qcursor (ts-query-cursor-new)))
    (print query)
    (print (ts-node-string root))
    (ts-query-cursor-exec qcursor (car query) root)
    ;; (print (ts-query-cursor-next-match ))
    (ts-query-cursor-delete qcursor)
    (ts-query-delete (car query))
    (ts-node-delete root)
    (ts-tree-delete tree)
    (ts-parser-delete parser)))
