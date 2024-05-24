(in-package :treesitter)

(defctype ts-state-id :uint16)
(defctype ts-symbol :uint16)
(defctype ts-field-id :uint16)

(defcenum ts-input-encoding
  +ts-input-encoding-utf8+
  +ts-input-encoding-utf16+)

(defcenum ts-symbol-type
  +ts-symbol-type-regular+
  +ts-symbol-type-anonymous+
  +ts-symbol-type-auxiliary+)

(defcenum ts-log-type
  +ts-log-type-parse+
  +ts-log-type-lex+)

(defcstruct ts-point
  (row :uint32)
  (column :uint32))

(defcenum ts-quantifier
  +ts-quantifier-zero+
  +ts-quantifier-zero-or-one+
  +ts-quantifier-zero-or-more+
  +ts-quantifier-one+
  +ts-quantifier-one-or-more+)

(defcstruct ts-range
  (start-point (:struct ts-point))
  (end-point (:struct ts-point))
  (start-byte :uint32)
  (end-byte :uint32))

(defcstruct ts-input
  (payload :pointer)
  (read :pointer)
  (encoding :int))

(defcstruct ts-logger
  (payload :pointer)
  (log :pointer))

(defcstruct ts-input-edit
  (start-byte :uint32)
  (old-end-byte :uint32)
  (new-end-byte :uint32)
  (start-point (:struct ts-point))
  (old-end-point (:struct ts-point))
  (new-end-point (:struct ts-point)))

(defcstruct ts-node
  (context :uint32 :count 4)
  (id :pointer)
  (tree :pointer))

(defcstruct ts-tree-cursor
  (tree :pointer)
  (id :pointer)
  (context :uint32 :count 3))

(defcstruct ts-query-capture
  (node (:struct ts-node))
  (index :uint32))
