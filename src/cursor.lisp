(in-package :treesitter)

(defcfun "ts_tree_cursor_new" (:struct ts-tree-cursor)
  (node (:struct ts-node)))

(defcfun "ts_tree_cursor_delete" :void
  (cursor (:pointer (:struct ts-tree-cursor))))

(defmacro with-ts-tree-cursor ((cursor node) &body body)
  `(let ((,cursor (ts-tree-cursor-new ,node)))
     (unwind-protect (progn ,@body)
       (ts-tree-cursor-delete ,cursor))))

(defcfun "ts_tree_cursor_reset" :void
  (cursor (:pointer (:struct ts-tree-cursor)))
  (node (:struct ts-node)))

(defcfun "ts_tree_cursor_reset_to" :void
  (dst (:pointer (:struct ts-tree-cursor)))
  (src (:pointer (:struct ts-tree-cursor))))

(defcfun ("ts_tree_cursor_current_node_" ts-tree-cursor-current-node) (:pointer (:struct ts-node))
  (cursor (:pointer (:struct ts-tree-cursor))))

(defmacro with-ts-tree-cursor-current-node ((node cursor) &body body)
  `(let ((,node (ts-tree-cursor-current-node ,cursor)))
     (unwind-protect (progn ,@body)
       (ts-node-delete ,node))))

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
