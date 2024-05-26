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
  (let ((parser (ts:make-parser :language *c-lang*)))
    (is
     (ts:node-string
      (ts:tree-root-node
       (ts:parser-parse-string parser "1+1;"))))
    (let ((root (ts:make-cursor
                 (ts:tree-root-node
                  (ts:parser-parse-string parser "a(b() + 12) - 1"))))))))

(test :cursor
  (let* ((parser (ts:make-parser :language *c-lang*))
         (tree (ts:parser-parse-string parser "a(b(c(d(e(f(g))))));"))
         (root (ts:tree-root-node tree))
         (cursor (ts:make-cursor root)))
    (ts:cursor-goto-first-child cursor)
    (ts:cursor-goto-first-child cursor)
    (is (equal 2 (ts:cursor-depth cursor)))))

(defun memtest ()
  (dotimes (i 1000000)
    (let* ((parser (ts:make-parser :language *c-lang*)))
      (ts:node-string
       (ts:tree-root-node
        (ts:parser-parse-string parser "1+1;")))))
  (tg:gc :full t))
