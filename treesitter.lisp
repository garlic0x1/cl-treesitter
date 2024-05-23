(defpackage :treesitter
  (:use :cl :cffi))
(in-package :treesitter)

(use-foreign-library "libtree-sitter.so")

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

(defcfun "ts_parser_parse_string" :pointer
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

(defcfun "ts_tree_root_node" (:struct ts-node)
  (tree :pointer))

(defcfun "ts_tree_root_node_with_offset" (:struct ts-node)
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

(defcfun "ts_node_type" :string
  (node (:struct ts-node)))

(defcfun "ts_node_symbol" ts-symbol
  (node (:struct ts-node)))

(defcfun "ts_node_language" :pointer
  (node (:struct ts-node)))

(defcfun "ts_node_grammar_type" :string
  (node (:struct ts-node)))

(defcfun "ts_node_grammar_symbol" ts-symbol
  (node (:struct ts-node)))

(defcfun "ts_node_start_byte" :uint32
  (node (:struct ts-node)))

(defcfun "ts_node_start_point" (:struct ts-point)
  (node (:struct ts-node)))

(defcfun "ts_node_end_byte" :uint32
  (node (:struct ts-node)))

(defcfun "ts_node_end_point" (:struct ts-point)
  (node (:struct ts-node)))

(defcfun "ts_node_string" :string
  (node (:struct ts-node)))

(defcfun "ts_node_is_null" :bool
  (node (:struct ts-node)))

(defcfun "ts_node_is_named" :bool
  (node (:struct ts-node)))

(defcfun "ts_node_is_extra" :bool
  (node (:struct ts-node)))

(defcfun "ts_node_has_changes" :bool
  (node (:struct ts-node)))

(defcfun "ts_node_has_error" :bool
  (node (:struct ts-node)))

(defcfun "ts_node_is_error" :bool
  (node (:struct ts-node)))

(defcfun "ts_node_parse_state" :int
  (node (:struct ts-node)))

(defcfun "ts_node_next_parse_state" :int
  (node (:struct ts-node)))

(defcfun "ts_node_parent" (:struct ts-node)
  (node (:struct ts-node)))

(defcfun "ts_node_child_containing_descendant" (:struct ts-node)
  (node (:struct ts-node))
  (descendant (:struct ts-node)))

(defcfun "ts_node_child" (:struct ts-node)
  (node (:struct ts-node))
  (child-index :uint32))

(defcfun "ts_node_field_name_for_child" :string
  (node (:struct ts-node))
  (child-index :uint32))

(defcfun "ts_node_child_count" :uint32
  (node (:struct ts-node)))

(defcfun "ts_node_named_child" (:struct ts-node)
  (node (:struct ts-node))
  (child-index :uint32))

(defcfun "ts_node_named_child_count" :uint32
  (node (:struct ts-node)))

(defcfun "ts_node_child_by_field_name" (:struct ts-node)
  (node (:struct ts-node))
  (name :string)
  (name-length :uint32))

(defcfun "ts_node_child_by_field_id" (:struct ts-node)
  (node (:struct ts-node))
  (field-id ts-field-id))

(defcfun "ts_node_next_sibling" (:struct ts-node)
  (node (:struct ts-node)))

(defcfun "ts_node_prev_sibling" (:struct ts-node)
  (node (:struct ts-node)))

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
