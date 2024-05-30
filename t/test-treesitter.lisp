(defpackage :treesitter/test
  (:use :cl :alexandria-2 :fiveam)
  (:local-nicknames (:ts :treesitter)))
(in-package :treesitter/test)

(def-suite :treesitter
  :description "Tests for the treesitter high-level API.")
(in-suite :treesitter)

(ts:include-language "c")
(defvar *c-lang* (tree-sitter-c))

(test :basic
  "Ensure parsing strings works."
  (let ((parser (ts:make-parser :language *c-lang*)))
    (is
     (ts:node-string
      (ts:tree-root-node
       (ts:parser-parse-string parser "1+1;"))))))

(test :cursor
  (let* ((parser (ts:make-parser :language *c-lang*))
         (tree (ts:parser-parse-string parser "a(b(c(d(e(f(g))))));"))
         (root (ts:tree-root-node tree))
         (cursor (ts:make-cursor root)))
    (ts:cursor-goto-first-child cursor)
    (ts:cursor-goto-first-child cursor)
    (is (equal 2 (ts:cursor-depth cursor)))))

(defparameter *query-code*
  "int function_one() { return 12; }
bool function_two() { return 0; }")

(test :query
  (let* ((parser (ts:make-parser :language *c-lang*))
         (tree (ts:parser-parse-string parser *query-code*))
         (root (ts:tree-root-node tree))
         (results (ts:query root "(return_statement) @param_expression")))
    (is (= 2 (length results))))
  (is (= 2 (line-up-first
            (ts:make-parser :language *c-lang*)
            (ts:parser-parse-string "int func() { return 0; return 1; }")
            (ts:tree-root-node)
            (ts:query "(return_statement) @x")
            (length)))))

(defun memtest ()
  (dotimes (i 500000)
    (let* ((parser (ts:make-parser :language *c-lang*)))
      (ts:node-string
       (ts:tree-root-node
        (ts:parser-parse-string parser "1+1;")))))
  (dotimes (i 5000)
    (line-up-first
     (ts:make-parser :language *c-lang*)
     (ts:parser-parse-string "int func() { return 0; return 1; }")
     (ts:tree-root-node)
     (ts:query "(return_statement) @x")
     (length)))
  (tg:gc :full t))
