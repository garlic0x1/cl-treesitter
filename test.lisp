(in-package :treesitter)

;; (defmacro ts-use-library (lang)
;;   `(progn
;;      (use-foreign-library ,(format nil "libtree-sitter-~(~a~).so" lang))
;;      (defcfun ,(format nil "tree_sitter_~(~a~)" lang) :pointer)))

;; (defun ts-language-new (lang)
;;   (funcall (intern (string-upcase (format nil "tree-sitter-~a" lang)))))

(ts-use-library :c)

(with-ts-parser (parser "c")
  (with-ts-tree (tree parser "1+1;")
    (with-ts-tree-root-node (node tree)
      (print (ts-node-string node)))
    ))
