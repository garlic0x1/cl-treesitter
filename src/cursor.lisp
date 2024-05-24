(defpackage :treesitter/cursor
  (:use :cl)
  (:export :make-ts-cursor
           ))
(in-package :treesitter/cursor)

(defun make-ts-cursor (node)
  (make-instance 'treesitter/types:ts-cursor
                 :pointer (treesitter/ffi::ts-tree-cursor-new node)
                 :free #'treesitter/ffi::ts-tree-cursor-delete))

(defun ts-cursor-current-node (cursor)
  (make-instance 'treesitter/types:ts-node
                 :pointer (treesitter/ffi::ts-tree-cursor-current-node cursor)
                 :free #'treesitter/ffi::ts-node-delete))
