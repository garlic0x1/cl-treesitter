(in-package :treesitter)

(defcfun ("ts_node_type_" ts-node-type) :string
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_symbol_" ts_node_symbol) ts-symbol
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_language_" ts-node-language) :pointer
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_grammar_type_" ts-node-grammar-type) :string
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_grammar_symbol_" ts-node-grammar-symbol) ts-symbol
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_start_byte_" ts-node-start-byte) :uint32
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_start_point_" ts-node-start-point) (:struct ts-point)
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_end_byte_" ts-node-end-byte) :uint32
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_end_point_" ts-node-end-point) (:struct ts-point)
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_string_" ts-node-string) :string
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_is_null_" ts-node-is-null) :bool
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_is_named_" ts-node-is-named) :bool
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_is_extra_" ts-node-is-extra) :bool
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_has_changes_" ts-node-has-changes) :bool
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_has_error_" ts-node-has-error) :bool
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_is_error_" ts-node-is-error) :bool
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_parse_state_" ts-node-parse-state) :int
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_next_parse_state_" ts-node-next-parse-state) :int
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_parent_" ts-node-parent!) (:pointer (:struct ts-node))
  (node (:pointer (:struct ts-node))))

(defmacro with-ts-node-parent ((parent node) &body body)
  `(let ((,parent (ts-node-parent! ,node)))
     (unwind-protect (progn ,@body)
       (ts-node-delete ,parent))))

(defcfun ("ts_node_child_containing_descendant_" ts-node-child-containint-descendant!) (:pointer (:struct ts-node))
  (node (:pointer (:struct ts-node)))
  (descendant (:struct ts-node)))

(defcfun ("ts_node_child_" ts-node-child!) (:pointer (:struct ts-node))
  (node (:pointer (:struct ts-node)))
  (child-index :uint32))

(defmacro with-ts-node-child ((child node child-index) &body body)
  `(let ((,child (ts-node-child! ,node ,child-index)))
     (unwind-protect (progn ,@body)
       (ts-node-delete ,child))))

(defcfun ("ts_node_field_name_for_child_" ts-node-field-name-for-child) :string
  (node (:pointer (:struct ts-node)))
  (child-index :uint32))

(defcfun ("ts_node_child_count_" ts-node-child-count) :uint32
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_named_child_" ts-node-named-child!) (:pointer (:struct ts-node))
  (node (:pointer (:struct ts-node)))
  (child-index :uint32))

(defcfun ("ts_node_named_child_count_" ts-node-named-child-count) :uint32
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_child_by_field_name_" ts-node-child-by-field-name!) (:pointer (:struct ts-node))
  (node (:pointer (:struct ts-node)))
  (name :string)
  (name-length :uint32))

(defcfun ("ts_node_child_by_field_id_" ts-node-child-by-field-id!) (:pointer (:struct ts-node))
  (node (:pointer (:struct ts-node)))
  (field-id ts-field-id))

(defcfun ("ts_node_next_sibling_" ts-node-next-sibling!) (:pointer (:struct ts-node))
  (node (:pointer (:struct ts-node))))

(defmacro with-ts-node-next-sibling ((next node) &body body)
  `(let ((,next (ts-node-next-sibling! ,node)))
     (unwind-protect (progn ,@body)
       (ts-node-delete ,next))))

(defcfun ("ts_node_prev_sibling_" ts-node-prev-sibling!) (:pointer (:struct ts-node))
  (node (:pointer (:struct ts-node))))

(defmacro with-ts-node-prev-sibling ((prev node) &body body)
  `(let ((,prev (ts-node-prev-sibling! ,node)))
     (unwind-protect (progn ,@body)
       (ts-node-delete ,prev))))

(defcfun ("ts_node_first_child_for_byte_" ts-node-first-child-for-byte!) (:pointer (:struct ts-node))
  (node (:pointer (:struct ts-node)))
  (byte :uint32))

(defcfun ("ts_node_first_named_child_for_byte_" ts-node-first-named-child-for-byte!) (:pointer (:struct ts-node))
  (node (:pointer (:struct ts-node)))
  (byte :uint32))

(defcfun ("ts_node_descendant_count_" ts-node-descendant-count) :uint32
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_descendant_for_byte_range_" ts-node-descendant-for-byte-range!) (:pointer (:struct ts-node))
  (node (:pointer (:struct ts-node)))
  (start :uint32)
  (end :uint32))

(defcfun ("ts_node_descendant_for_point_range_" ts-node-descendant-for-point-range!) (:pointer (:struct ts-node))
  (node (:pointer (:struct ts-node)))
  (start (:struct ts-point))
  (end (:struct ts-point)))

(defcfun ("ts_node_named_descendant_for_byte_range_" ts-node-named-descendant-for-byte-range!) (:pointer (:struct ts-node))
  (node (:pointer (:struct ts-node)))
  (start :uint32)
  (end :uint32))

(defcfun ("ts_node_named_descendant_for_point_range_" ts-node-named-descendant-for-point-range!) (:pointer (:struct ts-node))
  (node (:pointer (:struct ts-node)))
  (start (:struct ts-point))
  (end (:struct ts-point)))

(defcfun "ts_node_edit" :void
  (node (:pointer (:struct ts-node)))
  (edit :pointer))

(defcfun ("ts_node_eq_" ts-node-eq) :bool
  (node (:pointer (:struct ts-node)))
  (other (:pointer (:struct ts-node))))

(defcfun ts-node-delete :void
  (node (:pointer (:struct ts-node))))
