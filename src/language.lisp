(in-package :treesitter)

(defvar *ts-registry* (make-hash-table :test #'equal))

(defmacro ts-use-library (lang-spec)
  `(progn
     (use-foreign-library ,(format nil "libtree-sitter-~(~a~).so" lang-spec))
     (defcfun ,(format nil "tree_sitter_~(~a~)" lang-spec) :pointer)
     (setf (gethash ,lang-spec *ts-registry*)
           (intern (string-upcase (format nil "tree-sitter-~a" lang-spec))))))

(defun ts-language-new (lang-spec)
  (let ((constructor (gethash lang-spec *ts-registry*)))
    (if constructor
        (funcall constructor)
        (progn
          (ts-use-library lang-spec)
          (ts-language-new lang-spec)))))

(defun make-ts-language (lang-spec)
  (make-instance 'ts-language
                 :pointer (ts-language-new lang-spec)
                 :free #'ts/ffi::ts-language-delete))
