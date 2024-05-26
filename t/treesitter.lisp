(defpackage :treesitter/test
  (:use :cl :fiveam)
  (:local-nicknames (:ts :treesitter)))
(in-package :treesitter/test)

(def-suite :treesitter
  :description "Tests for the treesitter high-level API.")
(in-suite :treesitter)

(cffi:use-foreign-library "libtree-sitter-c.so")
(cffi:defcfun "tree_sitter_c" :pointer)
(defvar *c-lang* (tree-sitter-c))

(test :basic
  "Ensure parsing strings works."
  (let* ((parser (ts:make-parser :language *c-lang*)))
    (is
     (ts:node-string
      (ts:tree-root-node
       (ts:parser-parse-string parser "1+1;"))))))

(defun memtest ()
  (dotimes (i 1000000)
    (let* ((parser (ts:make-parser :language *c-lang*)))
      (ts:node-string
       (ts:tree-root-node
        (ts:parser-parse-string parser "1+1;")))))
  (tg:gc :full t))
