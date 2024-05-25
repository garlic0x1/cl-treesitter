(defpackage :treesitter
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
   :ts-point
   :ts-range
   :ts-input
   :ts-logger
   :ts-input-edit
   :ts-node
   :ts-tree-cursor
   :ts-query-capture
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
   :ts-node-is-missing
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
   :ts-node-next-named-sibling
   :ts-node-prev-named-sibling
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
(in-package :treesitter)

(use-foreign-library "libtree-sitter.so")

(define-foreign-library
    (shim :search-path (asdf:system-relative-pathname :treesitter ""))
  (t (:default "shim")))

(use-foreign-library shim)

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

(defun ts-parser-new (&key language timeout cancellation logger)
  "Create a new parser."
  (let ((parser (foreign-funcall "ts_parser_new" (:pointer))))
    (when language (ts-parser-set-language parser language))
    (when timeout (ts-parser-set-timeout-micros parser timeout))
    (when cancellation (ts-parser-set-cancellation-flag parser cancellation))
    (when logger (ts-parser-set-logger parser logger))
    parser))

(defcfun "ts_parser_delete" :void
  "Delete the parser, freeing all of the memory that it used."
  (parser :pointer))

(defcfun "ts_parser_language" :pointer
  "Get the parser's current language."
  (parser :pointer))

(defcfun "ts_parser_set_language" :bool
  "Set the language that the parser should use for parsing.

Returns a boolean indicating whether or not the language was successfully assigned."
  (parser :pointer)
  (language :pointer))

(defcfun "ts_parser_included_ranges" :pointer
  "Get the ranges of text that the parser will include when parsing.

The returned pointer is owned by the parser. The caller should not free it
or write to it. The length of the array will be written to the given
`count` pointer."
  (parser :pointer)
  (count (:pointer :uint32)))

(defcfun "ts_parser_set_included_ranges" :bool
  "Set the ranges of text that the parser should include when parsing.

By default, the parser will always include entire documents. This function
allows you to parse only a *portion* of a document but still return a syntax
tree whose ranges match up with the document as a whole. You can also pass
multiple disjoint ranges.

The second and third parameters specify the location and length of an array
of ranges. The parser does *not* take ownership of these ranges; it copies
the data, so it doesn't matter how these ranges are allocated.

If `count` is zero, then the entire document will be parsed. Otherwise,
the given ranges must be ordered from earliest to latest in the document,
and they must not overlap. That is, the following must hold for all:

`i < count - 1`: `ranges[i].end_byte <= ranges[i + 1].start_byte`

If this requirement is not satisfied, the operation will fail, the ranges
will not be assigned, and this function will return `false`. On success,
this function returns `true`"
  (parser :pointer)
  (ranges :pointer)
  (count :uint32))

(defcfun "ts_parser_parse" :pointer
  "Use the parser to parse some source code and create a syntax tree.

If you are parsing this document for the first time, pass `NULL` for the
`old_tree` parameter. Otherwise, if you have already parsed an earlier
version of this document and the document has since been edited, pass the
previous syntax tree so that the unchanged parts of it can be reused.
This will save time and memory. For this to work correctly, you must have
already edited the old syntax tree using the [`ts_tree_edit`] function in a
way that exactly matches the source code changes.

The [`TSInput`] parameter lets you specify how to read the text. It has the
following three fields:
1. [`read`]: A function to retrieve a chunk of text at a given byte offset
   and (row, column) position. The function should return a pointer to the
   text and write its length to the [`bytes_read`] pointer. The parser does
   not take ownership of this buffer; it just borrows it until it has
   finished reading it. The function should write a zero value to the
   [`bytes_read`] pointer to indicate the end of the document.
2. [`payload`]: An arbitrary pointer that will be passed to each invocation
   of the [`read`] function.
3. [`encoding`]: An indication of how the text is encoded. Either
   `TSInputEncodingUTF8` or `TSInputEncodingUTF16`.

This function returns a syntax tree on success, and `NULL` on failure. There
are three possible reasons for failure:
1. The parser does not have a language assigned. Check for this using the
   [`ts_parser_language`] function.
2. Parsing was cancelled due to a timeout that was set by an earlier call to
   the [`ts_parser_set_timeout_micros`] function. You can resume parsing from
   where the parser left out by calling [`ts_parser_parse`] again with the
   same arguments. Or you can start parsing from scratch by first calling
   [`ts_parser_reset`].
3. Parsing was cancelled using a cancellation flag that was set by an
   earlier call to [`ts_parser_set_cancellation_flag`]. You can resume parsing
   from where the parser left out by calling [`ts_parser_parse`] again with
   the same arguments.

[`read`]: TSInput::read
[`payload`]: TSInput::payload
[`encoding`]: TSInput::encoding
[`bytes_read`]: TSInput::read"
  (parser :pointer)
  (old-tree :pointer)
  (input (:pointer (:struct ts-input))))

(defun ts-parser-parse-string (parser string &optional (old-tree (null-pointer)))
  "Use the parser to parse some source code stored in one contiguous buffer.
The first two parameters are the same as in the [`ts_parser_parse`] function
above. The second two parameters indicate the location of the buffer and its
length in bytes."
  (foreign-funcall "ts_parser_parse_string"
                   :pointer parser
                   :pointer old-tree
                   :string string
                   :uint32 (length string)
                   :pointer))

(defun ts-parser-parse-string-encoded
    (parser string encoding &optional (old-tree (null-pointer)))
  "Use the parser to parse some source code stored in one contiguous buffer with
a given encoding. The first four parameters work the same as in the
[`ts_parser_parse_string`] method above. The final parameter indicates whether
the text is encoded as UTF8 or UTF16."
  (foreign-funcall "ts_parser_parse_string_encoded"
                   :pointer parser
                   :pointer old-tree
                   :string string
                   :uint32 (length string)
                   :int encoding
                   :pointer))

(defcfun "ts_parser_reset" :void
  "Instruct the parser to start the next parse from the beginning.

If the parser previously failed because of a timeout or a cancellation, then
by default, it will resume where it left off on the next call to
[`ts_parser_parse`] or other parsing functions. If you don't want to resume,
and instead intend to use this parser to parse some other document, you must
call [`ts_parser_reset`] first."
  (parser :pointer))

(defcfun "ts_parser_timeout_micros" :uint64
  "Get the duration in microseconds that parsing is allowed to take."
  (parser :pointer))

(defcfun "ts_parser_set_timeout_micros" :void
  "Set the maximum duration in microseconds that parsing should be allowed to
take before halting.

If parsing takes longer than this, it will halt early, returning NULL.
See [`ts_parser_parse`] for more information."
  (parser :pointer)
  (timeout-micros :uint64))

(defcfun "ts_parser_cancellation_flag" :int
  "Get the parser's current cancellation flag pointer."
  (parser :pointer))

(defcfun "ts_parser_set_cancellation_flag" :void
  "Set the parser's current cancellation flag pointer.

If a non-null pointer is assigned, then the parser will periodically read
from this pointer during parsing. If it reads a non-zero value, it will
halt early, returning NULL. See [`ts_parser_parse`] for more information."
  (parser :pointer)
  (flag :pointer))

(defcfun "ts_parser_set_logger" :void
  "Set the logger that a parser should use during parsing.

The parser does not take ownership over the logger payload. If a logger was
previously assigned, the caller is responsible for releasing any memory
owned by the previous logger."
  (parser :pointer)
  (logger :pointer))

(defcfun "ts_parser_logger" (:pointer (:struct ts-logger))
  "Get the parser's current logger."
  (parser :pointer))

(defcfun "ts_parser_print_dot_graphs" :void
  "Set the file descriptor to which the parser should write debugging graphs
during parsing. The graphs are formatted in the DOT language. You may want
to pipe these graphs directly to a `dot(1)` process in order to generate
SVG output. You can turn off this logging by passing a negative number."
  (parser :pointer)
  (fd :int))

;******************;
;* Section - Tree *;
;******************;

(defcfun "ts_tree_copy" :pointer
  "Create a shallow copy of the syntax tree. This is very fast.

You need to copy a syntax tree in order to use it on more than one thread at
a time, as syntax trees are not thread safe."
  (tree :pointer))

(defcfun "ts_tree_delete" :void
  "Delete the syntax tree, freeing all of the memory that it used."
  (tree :pointer))

(defcfun ("ts_tree_root_node_" ts-tree-root-node) (:pointer (:struct ts-node))
  "Get the root node of the syntax tree."
  (tree :pointer))

(defcfun ("ts_tree_root_node_with_offset_" ts-tree-root-node-with-offset) (:pointer (:struct ts-node))
  "Get the root node of the syntax tree, but with its position
shifted forward by the given offset."
  (tree :pointer)
  (offset-bytes :uint32)
  (offset-extent (:pointer (:struct ts-point))))

(defcfun "ts_tree_language" :pointer
  "Get the language that was used to parse the syntax tree."
  (tree :pointer))

(defcfun "ts_tree_included_ranges" (:pointer (:struct ts-range))
  "Get the array of included ranges that was used to parse the syntax tree.

The returned pointer must be freed by the caller."
  (tree :pointer)
  (length (:pointer :uint32)))

(defcfun "ts_tree_edit" :void
  "Edit the syntax tree to keep it in sync with source code that has been
edited.

You must describe the edit both in terms of byte offsets and in terms of
(row, column) coordinates."
  (tree :pointer)
  (edit :pointer))

(defcfun "ts_tree_get_changed_ranges" (:pointer (:struct ts-range))
  "Compare an old edited syntax tree to a new syntax tree representing the same
document, returning an array of ranges whose syntactic structure has changed.

For this to work correctly, the old syntax tree must have been edited such
that its ranges match up to the new tree. Generally, you'll want to call
this function right after calling one of the [`ts_parser_parse`] functions.
You need to pass the old tree that was passed to parse, as well as the new
tree that was returned from that function.

The returned array is allocated using `malloc` and the caller is responsible
for freeing it using `free`. The length of the array will be written to the
given `length` pointer."
  (old-tree :pointer)
  (new-tree :pointer)
  (length (:pointer :uint32)))

(defcfun "ts_tree_print_dot_graph" :void
  "Write a DOT graph describing the syntax tree to the given file."
  (tree :pointer)
  (fd :int))

;******************;
;* Section - Node *;
;******************;

(defcfun ("ts_node_type_" ts-node-type) :string
  "Get the node's type as a null-terminated string."
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_symbol_" ts_node_symbol) ts-symbol
  "Get the node's type as a numerical id."
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_language_" ts-node-language) :pointer
  "Get the node's language."
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_grammar_type_" ts-node-grammar-type) :string
  "Get the node's type as it appears in the grammar ignoring aliases as a
null-terminated string."
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_grammar_symbol_" ts-node-grammar-symbol) ts-symbol
  "Get the node's type as a numerical id as it appears in the grammar ignoring
aliases. This should be used in [`ts_language_next_state`] instead of
[`ts_node_symbol`]."
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_start_byte_" ts-node-start-byte) :uint32
  "Get the node's start byte."
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_start_point_" ts-node-start-point) (:pointer (:struct ts-point))
  "Get the node's start position in terms of rows and columns."
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_end_byte_" ts-node-end-byte) :uint32
  "Get the node's end byte."
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_end_point_" ts-node-end-point) (:pointer (:struct ts-point))
  "Get the node's end position in terms of rows and columns."
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_string_" ts-node-string) :string
  "Get an S-expression representing the node as a string.

This string is allocated with `malloc` and the caller is responsible for
freeing it using `free`."
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_is_null_" ts-node-is-null) :bool
  "Check if the node is null. Functions like [`ts_node_child`] and
[`ts_node_next_sibling`] will return a null node to indicate that no such node
was found."
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_is_named_" ts-node-is-named) :bool
  "Check if the node is *named*. Named nodes correspond to named rules in the
grammar, whereas *anonymous* nodes correspond to string literals in the
grammar."
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_is_missing_" ts-node-is-missing) :bool
  "Check if the node is *missing*. Missing nodes are inserted by the parser in
order to recover from certain kinds of syntax errors."
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_is_extra_" ts-node-is-extra) :bool
  "Check if the node is *extra*. Extra nodes represent things like comments,
which are not required the grammar, but can appear anywhere."
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_has_changes_" ts-node-has-changes) :bool
  "Check if a syntax node has been edited."
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_has_error_" ts-node-has-error) :bool
  "Check if the node is a syntax error or contains any syntax errors."
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_is_error_" ts-node-is-error) :bool
  "Check if the node is a syntax error."
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_parse_state_" ts-node-parse-state) :int
  "Get this node's parse state."
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_next_parse_state_" ts-node-next-parse-state) :int
  "Get the parse state after this node."
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_parent_" ts-node-parent) (:pointer (:struct ts-node))
  "Get the node's immediate parent.
Prefer [`ts_node_child_containing_descendant`] for
iterating over the node's ancestors."
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_child_containing_descendant_" ts-node-child-containint-descendant) (:pointer (:struct ts-node))
  "Get the node's child that contains `descendant`."
  (node (:pointer (:struct ts-node)))
  (descendant (:pointer (:struct ts-node))))

(defcfun ("ts_node_child_" ts-node-child) (:pointer (:struct ts-node))
  "Get the node's child at the given index, where zero represents the first child."
  (node (:pointer (:struct ts-node)))
  (child-index :uint32))

(defcfun ("ts_node_field_name_for_child_" ts-node-field-name-for-child) :string
  "Get the field name for node's child at the given index, where zero
represents the first child. Returns NULL, if no field is found."
  (node (:pointer (:struct ts-node)))
  (child-index :uint32))

(defcfun ("ts_node_child_count_" ts-node-child-count) :uint32
  "Get the node's number of children."
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_named_child_" ts-node-named-child) (:pointer (:struct ts-node))
  "Get the node's *named* child at the given index.

See also [`ts_node_is_named`]."
  (node (:pointer (:struct ts-node)))
  (child-index :uint32))

(defcfun ("ts_node_named_child_count_" ts-node-named-child-count) :uint32
  "Get the node's number of *named* children.

See also [`ts_node_is_named`]."
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_child_by_field_name_" ts-node-child-by-field-name) (:pointer (:struct ts-node))
  "Get the node's child with the given field name."
  (node (:pointer (:struct ts-node)))
  (name :string)
  (name-length :uint32))

(defcfun ("ts_node_child_by_field_id_" ts-node-child-by-field-id) (:pointer (:struct ts-node))
  "Get the node's child with the given numerical field id.

You can convert a field name to an id using the
[`ts_language_field_id_for_name`] function."
  (node (:pointer (:struct ts-node)))
  (field-id ts-field-id))

(defcfun ("ts_node_next_sibling_" ts-node-next-sibling) (:pointer (:struct ts-node))
  "Get the node's next sibling."
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_prev_sibling_" ts-node-prev-sibling) (:pointer (:struct ts-node))
  "Get the node's previous sibling."
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_next_named_sibling_" ts-node-next-named-sibling) (:pointer (:struct ts-node))
  "Get the node's next *named* sibling."
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_prev_named_sibling_" ts-node-prev-named-sibling) (:pointer (:struct ts-node))
  "Get the node's previous *named* sibling."
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_first_child_for_byte_" ts-node-first-child-for-byte) (:pointer (:struct ts-node))
  "Get the node's first child that extends beyond the given byte offset."
  (node (:pointer (:struct ts-node)))
  (byte :uint32))

(defcfun ("ts_node_first_named_child_for_byte_" ts-node-first-named-child-for-byte) (:pointer (:struct ts-node))
  "Get the node's first named child that extends beyond the given byte offset."
  (node (:pointer (:struct ts-node)))
  (byte :uint32))

(defcfun ("ts_node_descendant_count_" ts-node-descendant-count) :uint32
  "Get the node's number of descendants, including one for the node itself."
  (node (:pointer (:struct ts-node))))

(defcfun ("ts_node_descendant_for_byte_range_" ts-node-descendant-for-byte-range) (:pointer (:struct ts-node))
  "Get the smallest node within this node that spans the given range of bytes positions."
  (node (:pointer (:struct ts-node)))
  (start :uint32)
  (end :uint32))

(defcfun ("ts_node_descendant_for_point_range_" ts-node-descendant-for-point-range) (:pointer (:struct ts-node))
  "Get the smallest node within this node that spans the given range of (row, column) positions."
  (node (:pointer (:struct ts-node)))
  (start (:pointer (:struct ts-point)))
  (end (:pointer (:struct ts-point))))

(defcfun ("ts_node_named_descendant_for_byte_range_" ts-node-named-descendant-for-byte-range) (:pointer (:struct ts-node))
  "Get the smallest named node within this node that spans the given range of bytes positions."
  (node (:pointer (:struct ts-node)))
  (start :uint32)
  (end :uint32))

(defcfun ("ts_node_named_descendant_for_point_range_" ts-node-named-descendant-for-point-range) (:pointer (:struct ts-node))
  "Get the smallest named node within this node that spans the given range of (row, column) positions."
  (node (:pointer (:struct ts-node)))
  (start (:pointer (:struct ts-point)))
  (end (:pointer (:struct ts-point))))

(defcfun "ts_node_edit" :void
  "Edit the node to keep it in-sync with source code that has been edited.

This function is only rarely needed. When you edit a syntax tree with the
[`ts_tree_edit`] function, all of the nodes that you retrieve from the tree
afterward will already reflect the edit. You only need to use [`ts_node_edit`]
when you have a [`TSNode`] instance that you want to keep and continue to use
after an edit."
  (node (:pointer (:struct ts-node)))
  (edit :pointer))

(defcfun ("ts_node_eq_" ts-node-eq) :bool
  "Check if two nodes are identical."
  (node (:pointer (:struct ts-node)))
  (other (:pointer (:struct ts-node))))

(defcfun ts-node-delete :void
  "Delete the node, freeing all of the memory that it used."
  (node (:pointer (:struct ts-node))))

;************************;
;* Section - TreeCursor *;
;************************;

(defcfun ("ts_tree_cursor_new_" ts-tree-cursor-new) (:pointer (:struct ts-tree-cursor))
  "Create a new tree cursor starting from the given node.

A tree cursor allows you to walk a syntax tree more efficiently than is
possible using the [`TSNode`] functions. It is a mutable object that is always
on a certain syntax node, and can be moved imperatively to different nodes."
  (node (:pointer (:struct ts-node))))

(defcfun "ts_tree_cursor_delete" :void
  "Delete a tree cursor, freeing all of the memory that it used."
  (cursor (:pointer (:struct ts-tree-cursor))))

(defcfun "ts_tree_cursor_reset" :void
  "Re-initialize a tree cursor to start at a different node."
  (cursor (:pointer (:struct ts-tree-cursor)))
  (node (:pointer (:struct ts-node))))

(defcfun "ts_tree_cursor_reset_to" :void
  "Re-initialize a tree cursor to the same position as another cursor.

Unlike [`ts_tree_cursor_reset`], this will not lose parent information and
allows reusing already created cursors."
  (dst (:pointer (:struct ts-tree-cursor)))
  (src (:pointer (:struct ts-tree-cursor))))

(defcfun ("ts_tree_cursor_current_node_" ts-tree-cursor-current-node) (:pointer (:struct ts-node))
  "Get the tree cursor's current node."
  (cursor (:pointer (:struct ts-tree-cursor))))

(defcfun "ts_tree_cursor_current_field_name" :string
  "Get the field name of the tree cursor's current node.

This returns `NULL` if the current node doesn't have a field.
See also [`ts_node_child_by_field_name`]."
  (cursor (:pointer (:struct ts-tree-cursor))))

(defcfun "ts_tree_cursor_current_field_id" ts-field-id
  "Get the field id of the tree cursor's current node.

This returns zero if the current node doesn't have a field.
See also [`ts_node_child_by_field_id`], [`ts_language_field_id_for_name`]."
  (cursor (:pointer (:struct ts-tree-cursor))))

(defcfun "ts_tree_cursor_goto_parent" :bool
  "Move the cursor to the parent of its current node.

This returns `true` if the cursor successfully moved, and returns `false`
if there was no parent node (the cursor was already on the root node)."
  (cursor (:pointer (:struct ts-tree-cursor))))

(defcfun "ts_tree_cursor_goto_next_sibling" :bool
  "Move the cursor to the next sibling of its current node.

This returns `true` if the cursor successfully moved, and returns `false`
if there was no next sibling node."
  (cursor (:pointer (:struct ts-tree-cursor))))

(defcfun "ts_tree_cursor_goto_previous_sibling" :bool
  "Move the cursor to the previous sibling of its current node.

This returns `true` if the cursor successfully moved, and returns `false` if
there was no previous sibling node.

Note, that this function may be slower than
[`ts_tree_cursor_goto_next_sibling`] due to how node positions are stored. In
the worst case, this will need to iterate through all the children upto the
previous sibling node to recalculate its position."
  (cursor (:pointer (:struct ts-tree-cursor))))

(defcfun "ts_tree_cursor_goto_first_child" :bool
  "Move the cursor to the first child of its current node.

This returns `true` if the cursor successfully moved, and returns `false`
if there were no children."
  (cursor (:pointer (:struct ts-tree-cursor))))

(defcfun "ts_tree_cursor_goto_last_child" :bool
  "Move the cursor to the last child of its current node.

This returns `true` if the cursor successfully moved, and returns `false` if
there were no children.

Note that this function may be slower than [`ts_tree_cursor_goto_first_child`]
because it needs to iterate through all the children to compute the child's
position."
  (cursor (:pointer (:struct ts-tree-cursor))))

(defcfun "ts_tree_cursor_goto_descendant" :bool
  "Move the cursor to the node that is the nth descendant of
the original node that the cursor was constructed with, where
zero represents the original node itself."
  (cursor (:pointer (:struct ts-tree-cursor)))
  (goal-descendant-index :uint32))

(defcfun "ts_tree_cursor_current_descendant_index" :uint32
  "Get the index of the cursor's current node out of all of the
descendants of the original node that the cursor was constructed with."
  (cursor (:pointer (:struct ts-tree-cursor))))

(defcfun "ts_tree_cursor_current_depth" :uint32
  "Get the depth of the cursor's current node relative to the original
node that the cursor was constructed with."
  (cursor (:pointer (:struct ts-tree-cursor))))

(defcfun "ts_tree_cursor_goto_first_child_for_byte" :uint64
  "Move the cursor to the first child of its current node that extends beyond
the given byte offset.

This returns the index of the child node if one was found, and returns -1
if no such child was found."
  (cursor (:pointer (:struct ts-tree-cursor)))
  (goal-byte :uint32))

(defcfun "ts_tree_cursor_goto_first_child_for_point" :uint64
  "Move the cursor to the first child of its current node that extends beyond
the given point.

This returns the index of the child node if one was found, and returns -1
if no such child was found."
  (cursor (:pointer (:struct ts-tree-cursor)))
  (goal-point (:pointer (:struct ts-point))))

;**********************;
;* Section - Language *;
;**********************;

(defcfun "ts_language_copy" :pointer
  "Get another reference to the given language."
  (language :pointer))

(defcfun "ts_language_delete" :void
  "Free any dynamically-allocated resources for this language, if
this is the last reference."
  (language :pointer))

(defcfun "ts_language_symbol_count" :uint32
  "Get the number of distinct node types in the language."
  (language :pointer))

(defcfun "ts_language_state_count" :uint32
  "Get the number of valid states in this language."
  (language :pointer))

(defcfun "ts_language_symbol_name" :string
  "Get a node type string for the given numerical id."
  (language :pointer)
  (symbol ts-symbol))

(defcfun "ts_language_symbol_for_name" ts-symbol
  "Get the numerical id for the given node type string."
  (language :pointer)
  (string :string)
  (length :uint32)
  (named-p :bool))

(defcfun "ts_language_field_count" :uint32
  "Get the number of distinct field names in the language."
  (language :pointer))

(defcfun "ts_language_field_name_for_id" :string
  "Get the field name string for the given numerical id."
  (language :pointer)
  (id ts-field-id))

(defcfun "ts_language_field_id_for_name" ts-field-id
  "Get the numerical id for the given field name string."
  (language :pointer)
  (name :string)
  (length :uint32))

(defcfun "ts_language_symbol_type" ts-symbol-type
  "Check whether the given node type id belongs to named nodes, anonymous nodes,
or a hidden nodes.

See also [`ts_node_is_named`]. Hidden nodes are never returned from the API."
  (language :pointer)
  (symbol ts-symbol))

(defcfun "ts_language_version" :uint32
  "Get the ABI version number for this language. This version number is used
to ensure that languages were generated by a compatible version of
Tree-sitter.

See also [`ts_parser_set_language`]."
  (language :pointer))

(defcfun "ts_language_next_state" ts-state-id
  "Get the next parse state. Combine this with lookahead iterators to generate
completion suggestions or valid symbols in error nodes. Use
[`ts_node_grammar_symbol`] for valid symbols."
  (language :pointer)
  (state ts-state-id)
  (symbol ts-symbol))
