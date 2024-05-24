(in-package :treesitter)

(defcfun "ts_language_copy" :pointer
  (language :pointer))

(defcfun "ts_language_delete" :void
  (language :pointer))

(defcfun "ts_language_symbol_count" :uint32
  (language :pointer))

(defcfun "ts_language_state_count" :uint32
  (language :pointer))

(defcfun "ts_language_symbol_name" :string
  (language :pointer)
  (symbol ts-symbol))

(defcfun "ts_language_symbol_for_name" ts-symbol
  (language :pointer)
  (string :string)
  (length :uint32)
  (named-p :bool))

(defcfun "ts_language_field_count" :uint32
  (language :pointer))

(defcfun "ts_language_field_name_for_id" :string
  (language :pointer)
  (id ts-field-id))

(defcfun "ts_language_field_id_for_name" ts-field-id
  (language :pointer)
  (name :string)
  (length :uint32))

(defcfun "ts_language_symbol_type" ts-symbol-type
  (language :pointer)
  (symbol ts-symbol))

(defcfun "ts_language_version" :uint32
  (language :pointer))

(defcfun "ts_language_next_state" ts-state-id
  (language :pointer)
  (state ts-state-id)
  (symbol ts-symbol))

(defmacro ts-use-library (lang)
  `(progn
     (use-foreign-library ,(format nil "libtree-sitter-~(~a~).so" lang))
     (defcfun ,(format nil "tree_sitter_~(~a~)" lang) :pointer)))

(defun ts-language-new (lang)
  (funcall (intern (string-upcase (format nil "tree-sitter-~a" lang)))))

(defmacro with-ts-language ((lang lang-string) &body body)
  `(let ((,lang (ts-language-new ,lang-string)))
     (unwind-protect (progn ,@body)
       (ts-language-delete ,lang))))
