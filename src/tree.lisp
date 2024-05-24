(in-package :treesitter)

(defcfun "ts_tree_copy" :pointer
  (tree :pointer))

(defcfun "ts_tree_delete" :void
  (tree :pointer))

(defcfun ("ts_tree_root_node_" ts-tree-root-node!) (:pointer (:struct ts-node))
  (tree :pointer))

(defmacro with-ts-tree-root-node ((node tree) &body body)
  `(let ((,node (ts-tree-root-node! ,tree)))
     (unwind-protect (progn ,@body)
       (ts-node-delete ,node))))

(defcfun ("ts_tree_root_node_with_offset_" ts-tree-root-node-with-offset!) (:pointer (:struct ts-node))
  (tree :pointer)
  (offset-bytes :uint32)
  (offset-extent (:struct ts-point)))

(defmacro with-ts-tree-root-node-with-offset ((node tree offset-bytes offset-extent) &body body)
  `(let ((,node (ts-tree-root-node-with-offset! ,tree ,offset-bytes ,offset-extent)))
     (unwind-protect (progn ,@body)
       (ts-node-delete ,node))))

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
