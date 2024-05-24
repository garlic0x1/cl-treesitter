(defpackage :treesitter
  (:use :cl :cffi)
  (:export
   ;; parser.lisp
   :with-ts-parser
   ;; :ts-parser-parse-string
   :with-ts-tree
   :ts-parser-language
   :ts-parser-timeout-micros
   :ts-parser-cancellation-flag
   :ts-parser-reset
   :ts-parser-print-dot-graphs
   ;; language.lisp
   :ts-use-language
   :with-ts-language
   ;; tree.lisp
   :with-ts-tree-root-node
   :with-ts-tree-root-node-with-offset
   :ts-tree-language
   :ts-tree-print-dot-graph
   ;; node.lisp
   :ts-node-type
   :ts-node-symbol
   :ts-node-language
   :ts-node-grammar-type
   :ts-node-grammar-symbol
   :ts-node-start-byte
   :ts-node-start-point
   :ts-node-end-byte
   :ts-node-end-point
   :ts-node-string
   :ts-node-is-null
   :ts-node-is-named
   :ts-node-is-extra
   :ts-node-has-changes
   :ts-node-has-error
   :ts-node-is-error
   :ts-node-parse-state
   :ts-node-next-parse-state
   :with-ts-node-parent
   :with-ts-node-child
   :with-ts-node-prev-sibling
   :with-ts-node-next-sibling
   :ts-node-child-count
   :ts-node-named-child-count
   :ts-node-eq
   ;; cursor.lisp
   :with-ts-tree-cursor
   :ts-tree-cursor-reset
   :with-ts-tree-cursor-current-node
   :ts-tree-cursor-current-field-name
   :ts-tree-cursor-current-field-id
   :ts-tree-cursor-goto-parent
   :ts-tree-cursor-goto-next-sibling
   :ts-tree-cursor-goto-previous-sibling
   :ts-tree-cursor-goto-first-child
   :ts-tree-cursor-goto-last-child
   :ts-tree-cursor-current-depth))
(in-package :treesitter)

(use-foreign-library "libtree-sitter.so")

(define-foreign-library
    (shim :search-path (asdf:system-relative-pathname :cl-treesitter ""))
  (t (:default "shim")))

(use-foreign-library shim)
