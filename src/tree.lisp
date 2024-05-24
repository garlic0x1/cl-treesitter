(defpackage :treesitter/tree
  (:use :cl :treesitter/types)
  (:import-from :treesitter/ffi
                :ts-tree-language
                :ts-tree-print-dot-graph)
  (:export :ts-tree-root-node
           :ts-tree-language
           :ts-tree-print-dot-graph))
(in-package :treesitter/tree)

(defun ts-tree-root-node (tree &key offset-bytes offset-extent)
  (make-instance 'ts-node
                 :pointer (if (and offset-bytes offset-extent)
                              (treesitter/ffi::ts-tree-root-node-with-offset
                               tree offset-bytes offset-extent)
                              (treesitter/ffi::ts-tree-root-node tree))
                 :free #'treesitter/ffi::ts-node-delete))
