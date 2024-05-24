(defpackage :treesitter/ffi
  (:use :cl :cffi)
  (:export
   ;; types
   :+ts-input-encoding-utf8+
   :+ts-input-encoding-utf16+
   :+ts-symbol-type-regular+
   :+ts-symbol-type-anonymous+
   :+ts-symbol-type-auxiliary+
   :+ts-log-type-parse+
   :+ts-log-type-lex+
   :+ts-quantifier-zero+
   :+ts-quantifier-zero-or-one+
   :+ts-quantifier-zero-or-more+
   :+ts-quantifier-one+
   :+ts-quantifier-one-or-more+
   ;; parser
   :ts-parser-new
   :ts-parser-delete
   :ts-parser-language
   :ts-parser-set-language
   :ts-parser-included-ranges
   :ts-parser-set-included-ranges
   :ts-parser-parse
   :ts-parser-parse-string
   :ts-parser-parse-string-encoded
   :ts-parser-reset
   :ts-parser-timeout-micros
   :ts-parser-set-timeout-micros
   :ts-parser-cancellation-flag
   :ts-parser-set-cancellation-flag
   :ts-parser-logger
   :ts-parser-print-dot-graphs
   ;; tree
   :ts-tree-copy
   :ts-tree-delete
   :ts-tree-root-node
   :ts-tree-root-node-with-offset
   :ts-tree-language
   :ts-tree-included-ranges
   :ts-tree-edit
   :ts-tree-get-changed-ranges
   :ts-tree-print-dot-graph
   ;; node
   :ts-node-delete
   :ts-node-type
   :ts-node-symbol
   :ts-node-language
   :ts-node-grammar-type
   :ts-node-grammar-symbol
   :ts-node-start-byte
   :ts-node-end-byte
   :ts-node-start-point
   :ts-node-end-point
   :ts-node-string
   :ts-node-is-null
   :ts-node-is-named
   :ts-node-is-extra
   :ts-node-has-changes
   :ts-node-has-error
   :ts-node-is-error
   :ts-node-parse-state
   :ts-node-next-parse-state
   :ts-node-parent
   :ts-node-child-containint-descendant
   :ts-node-child
   :ts-node-field-name-for-child
   :ts-node-child-count
   :ts-node-named-child
   :ts-node-named-child-count
   :ts-node-child-by-field-name
   :ts-node-child-by-field-id
   :ts-node-next-sibling
   :ts-node-prev-sibling
   :ts-node-first-child-for-byte
   :ts-node-first-named-child-for-byte
   :ts-node-descendant-count
   :ts-node-descendant-for-byte-range
   :ts-node-descendant-for-point-range
   :ts-node-named-descendant-for-byte-range
   :ts-node-named-descendant-for-point-range
   :ts-node-edit
   :ts-node-eq
   ;; cursor
   :ts-tree-cursor-new
   :ts-tree-cursor-delete
   :ts-tree-cursor-reset
   :ts-tree-cursor-reset-to
   :ts-tree-cursor-current-node
   :ts-tree-cursor-current-field-name
   :ts-tree-cursor-current-field-id
   :ts-tree-cursor-goto-parent
   :ts-tree-cursor-goto-next-sibling
   :ts-tree-cursor-goto-previous-sibling
   :ts-tree-cursor-goto-first-child
   :ts-tree-cursor-goto-last-child
   :ts-tree-cursor-goto-descendant
   :ts-tree-cursor-current-descendant-index
   :ts-tree-cursor-current-depth
   :ts-tree-cursor-goto-first-child-for-byte
   :ts-tree-cursor-goto-first-child-for-point
   ;; language
   :ts-language-copy
   :ts-language-delete
   :ts-language-symbol-count
   :ts-language-state-count
   :ts-language-symbol-name
   :ts-language-symbol-for-name
   :ts-language-field-count
   :ts-language-field-name-for-id
   :ts-language-field-id-for-name
   :ts-language-symbol-type
   :ts-language-version
   :ts-language-next-state
   ))
(in-package :treesitter/ffi)

;*******************;
;* Section - Types *;
;*******************;

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

(defcenum ts-quantifier
  +ts-quantifier-zero+
  +ts-quantifier-zero-or-one+
  +ts-quantifier-zero-or-more+
  +ts-quantifier-one+
  +ts-quantifier-one-or-more+)

(defcstruct ts-point
  (row :uint32)
  (column :uint32))

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

;********************;
;* Section - Parser *;
;********************;

(defcfun "ts_parser_new" :pointer)

(defcfun "ts_parser_delete" :void
  (parser :pointer))

(defcfun "ts_parser_language" :pointer
  (parser :pointer))

(defcfun "ts_parser_set_language" :bool
  (parser :pointer)
  (language :pointer))

(defcfun "ts_parser_included_ranges" :pointer
  (parser :pointer)
  (count (:pointer :uint32)))

(defcfun "ts_parser_set_included_ranges" :bool
  (parser :pointer)
  (ranges :pointer)
  (count :uint32))

(defcfun "ts_parser_parse" :pointer
  (parser :pointer)
  (old-tree :pointer)
  (input (:struct ts-input)))

(defcfun ("ts_parser_parse_string" ts-parser-parse-string*) :pointer
  (parser :pointer)
  (old-tree :pointer)
  (string :string)
  (length :uint32))

(defcfun "ts_parser_parse_string_encoded" :pointer
  (parser :pointer)
  (old-tree :pointer)
  (string :string)
  (length :uint32)
  (encoding :int))

(defcfun "ts_parser_reset" :void
  (parser :pointer))

(defcfun "ts_parser_timeout_micros" :uint64
  (parser :pointer))

(defcfun "ts_parser_set_timeout_micros" :void
  (parser :pointer)
  (timeout-micros :uint64))

(defcfun "ts_parser_cancellation_flag" :int
  (parser :pointer))

(defcfun "ts_parser_set_cancellation_flag" :void
  (parser :pointer)
  (flag :pointer))

(defcfun "ts_parser_logger" (:struct ts-logger)
  (parser :pointer))

(defcfun "ts_parser_print_dot_graphs" :void
  (parser :pointer)
  (fd :int))

;******************;
;* Section - Tree *;
;******************;

(defcfun "ts_tree_copy" :pointer
  (tree :pointer))

(defcfun "ts_tree_delete" :void
  (tree :pointer))

(defcfun ("ts_tree_root_node_" ts-tree-root-node) (:pointer (:struct ts-node))
  (tree :pointer))

(defcfun ("ts_tree_root_node_with_offset_" ts-tree-root-node-with-offset) (:pointer (:struct ts-node))
  (tree :pointer)
  (offset-bytes :uint32)
  (offset-extent (:struct ts-point)))

(defcfun "ts_tree_language" :pointer
  (tree :pointer))

(defcfun "ts_tree_included_ranges" (:pointer (:struct ts-range))
  (tree :pointer)
  (length (:pointer :uint32)))

(defcfun "ts_tree_edit" :void
  (tree :pointer)
  (edit :pointer))

(defcfun "ts_tree_get_changed_ranges" (:pointer (:struct ts-range))
  (old-tree :pointer)
  (new-tree :pointer)
  (length (:pointer :uint32)))

(defcfun "ts_tree_print_dot_graph" :void
  (tree :pointer)
  (fd :int))

;******************;
;* Section - Node *;
;******************;

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

(defcfun ("ts_node_parent_" ts-node-parent) (:pointer (:struct ts-node))
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_child_containing_descendant_" ts-node-child-containint-descendant) (:pointer (:struct ts-node))
  (node (:pointer (:struct ts-node)))
  (descendant (:struct ts-node)))

(defcfun ("ts_node_child_" ts-node-child) (:pointer (:struct ts-node))
  (node (:pointer (:struct ts-node)))
  (child-index :uint32))

(defcfun ("ts_node_field_name_for_child_" ts-node-field-name-for-child) :string
  (node (:pointer (:struct ts-node)))
  (child-index :uint32))

(defcfun ("ts_node_child_count_" ts-node-child-count) :uint32
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_named_child_" ts-node-named-child) (:pointer (:struct ts-node))
  (node (:pointer (:struct ts-node)))
  (child-index :uint32))

(defcfun ("ts_node_named_child_count_" ts-node-named-child-count) :uint32
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_child_by_field_name_" ts-node-child-by-field-name) (:pointer (:struct ts-node))
  (node (:pointer (:struct ts-node)))
  (name :string)
  (name-length :uint32))

(defcfun ("ts_node_child_by_field_id_" ts-node-child-by-field-id) (:pointer (:struct ts-node))
  (node (:pointer (:struct ts-node)))
  (field-id ts-field-id))

(defcfun ("ts_node_next_sibling_" ts-node-next-sibling) (:pointer (:struct ts-node))
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_prev_sibling_" ts-node-prev-sibling) (:pointer (:struct ts-node))
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_first_child_for_byte_" ts-node-first-child-for-byte) (:pointer (:struct ts-node))
  (node (:pointer (:struct ts-node)))
  (byte :uint32))

(defcfun ("ts_node_first_named_child_for_byte_" ts-node-first-named-child-for-byte) (:pointer (:struct ts-node))
  (node (:pointer (:struct ts-node)))
  (byte :uint32))

(defcfun ("ts_node_descendant_count_" ts-node-descendant-count) :uint32
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_descendant_for_byte_range_" ts-node-descendant-for-byte-range) (:pointer (:struct ts-node))
  (node (:pointer (:struct ts-node)))
  (start :uint32)
  (end :uint32))

(defcfun ("ts_node_descendant_for_point_range_" ts-node-descendant-for-point-range) (:pointer (:struct ts-node))
  (node (:pointer (:struct ts-node)))
  (start (:struct ts-point))
  (end (:struct ts-point)))

(defcfun ("ts_node_named_descendant_for_byte_range_" ts-node-named-descendant-for-byte-range) (:pointer (:struct ts-node))
  (node (:pointer (:struct ts-node)))
  (start :uint32)
  (end :uint32))

(defcfun ("ts_node_named_descendant_for_point_range_" ts-node-named-descendant-for-point-range) (:pointer (:struct ts-node))
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

;************************;
;* Section - TreeCursor *;
;************************;

(defcfun "ts_tree_cursor_new" (:pointer (:struct ts-tree-cursor))
  (node (:pointer (:struct ts-node))))

(defcfun "ts_tree_cursor_delete" :void
  (cursor (:pointer (:struct ts-tree-cursor))))

(defcfun "ts_tree_cursor_reset" :void
  (cursor (:pointer (:struct ts-tree-cursor)))
  (node (:struct ts-node)))

(defcfun "ts_tree_cursor_reset_to" :void
  (dst (:pointer (:struct ts-tree-cursor)))
  (src (:pointer (:struct ts-tree-cursor))))

(defcfun ("ts_tree_cursor_current_node_" ts-tree-cursor-current-node) (:pointer (:struct ts-node))
  (cursor (:pointer (:struct ts-tree-cursor))))

(defcfun "ts_tree_cursor_current_field_name" :string
  (cursor (:pointer (:struct ts-tree-cursor))))

(defcfun "ts_tree_cursor_current_field_id" ts-field-id
  (cursor (:pointer (:struct ts-tree-cursor))))

(defcfun "ts_tree_cursor_goto_parent" :bool
  (cursor (:pointer (:struct ts-tree-cursor))))

(defcfun "ts_tree_cursor_goto_next_sibling" :bool
  (cursor (:pointer (:struct ts-tree-cursor))))

(defcfun "ts_tree_cursor_goto_previous_sibling" :bool
  (cursor (:pointer (:struct ts-tree-cursor))))

(defcfun "ts_tree_cursor_goto_first_child" :bool
  (cursor (:pointer (:struct ts-tree-cursor))))

(defcfun "ts_tree_cursor_goto_last_child" :bool
  (cursor (:pointer (:struct ts-tree-cursor))))

(defcfun "ts_tree_cursor_goto_descendant" :bool
  (cursor (:pointer (:struct ts-tree-cursor)))
  (goal-descendant-index :uint32))

(defcfun "ts_tree_cursor_current_descendant_index" :uint32
  (cursor (:pointer (:struct ts-tree-cursor))))

(defcfun "ts_tree_cursor_current_depth" :uint32
  (cursor (:pointer (:struct ts-tree-cursor))))

(defcfun "ts_tree_cursor_goto_first_child_for_byte" :uint64
  (cursor (:pointer (:struct ts-tree-cursor)))
  (goal-byte :uint32))

(defcfun "ts_tree_cursor_goto_first_child_for_point" :uint64
  (cursor (:pointer (:struct ts-tree-cursor)))
  (goal-point (:struct ts-point)))

;**********************;
;* Section - Language *;
;**********************;

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
