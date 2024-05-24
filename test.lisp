(in-package :treesitter)

(defmacro ts-use-library (lang)
  `(progn
     (use-foreign-library ,(format nil "libtree-sitter-~(~a~).so" lang))
     (defcfun ,(format nil "tree_sitter_~(~a~)" lang) :pointer)))

(defun ts-language-new (lang)
  (funcall (intern (string-upcase (format nil "tree-sitter-~a" lang)))))

(ts-use-library :c)

(let* ((parser (ts-parser-new))
       (language (tree-sitter-c)))
  (ts-parser-set-language parser language)
  (let* ((parsed (ts-parser-parse-string parser (cffi:null-pointer) "1+1;" 4))
         (root (ts-tree-root-node parsed)))
    (print root)
    (ts-node-string root)))
