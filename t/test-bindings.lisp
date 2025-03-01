(defpackage #:treesitter/test/bindings
  (:use #:cl #:cffi #:fiveam #:treesitter/bindings))
(in-package #:treesitter/test/bindings)

(def-suite :treesitter/bindings
  :description "Tests for treesitter low-level bindings.")
(in-suite :treesitter/bindings)

(cffi:use-foreign-library #-darwin "libtree-sitter-c.so"
                          #+darwin "libtree-sitter-c.dylib")
(cffi:defcfun "tree_sitter_c" :pointer)
(defvar *c-lang* (tree-sitter-c))

(test :basic
  "Ensure parsing strings works."
  ;; alloc
  (let* ((parser (ts-parser-new :language *c-lang*))
         (tree (ts-parser-parse-string parser "1 + 1;"))
         (root (ts-tree-root-node tree)))
    (is (equal
         "(translation_unit (expression_statement (update_expression argument: (binary_expression left: (number_literal) right: (number_literal)) operator: (MISSING \"--\"))))"
         (ts-node-string root)))
    ;; free
    (ts-node-delete root)
    (ts-tree-delete tree)
    (ts-parser-delete parser)))

(test :cursor
  "Test creating and moving a cursor."
  ;; alloc
  (let* ((parser (ts-parser-new :language *c-lang*))
         (tree (ts-parser-parse-string parser "bool test_cursors(539);"))
         (root (ts-tree-root-node tree))
         (cursor (ts-tree-cursor-new root)))
    (ts-tree-cursor-goto-first-child cursor)
    (ts-tree-cursor-goto-first-child cursor)
    (is (equal "type" (ts-tree-cursor-current-field-name cursor)))
    ;; alloc
    (let ((n (ts-tree-cursor-current-node cursor)))
      (is (equal "primitive_type"
                 (ts-language-symbol-name *c-lang* (ts-node-symbol n))))
      ;; free
      (ts-node-delete n))
    ;; free
    (ts-tree-cursor-delete cursor)
    (ts-node-delete root)
    (ts-tree-delete tree)
    (ts-parser-delete parser)))

(test :query
  ;; alloc
  (let* ((parser (ts-parser-new :language *c-lang*))
         (source "int main() { return 0; }")
         (query-string "(return_statement) @param_expression")
         (tree (ts-parser-parse-string parser source))
         (root (ts-tree-root-node tree))
         (query (ts-query-new *c-lang* query-string))
         (qcursor (ts-query-cursor-new)))
    (ts-query-cursor-exec qcursor query root)
    ;; alloc
    (let ((match (ts-query-match-new)))
      (is (ts-query-cursor-next-match qcursor match))
      ;; alloc
      (let* ((capture (ts-query-match-capture match 0))
             (node (ts-query-capture-node capture)))
        (is (equal "(return_statement (number_literal))" (ts-node-string node)))
        (is (equal :none (ts-query-error-type query)))
        ;; free
        (ts-node-delete node)
        (ts-query-capture-delete capture))
      ;; free
      (ts-query-match-delete match))
    ;; free
    (ts-query-cursor-delete qcursor)
    (ts-query-delete query)
    (ts-node-delete root)
    (ts-tree-delete tree)
    (ts-parser-delete parser)))
