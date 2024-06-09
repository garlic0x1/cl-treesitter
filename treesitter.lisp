(defpackage :treesitter
  (:use :cl :cffi :treesitter/bindings)
  (:export
   ;; types
   :language
   :parser
   :point
   :node
   :tree
   :cursor
   ;; parser
   :make-parser
   :parser-language
   :parser-timeout
   :parser-parse-string
   :parser-reset
   ;; tree
   :tree-root-node
   :tree-language
   ;; node
   :node-type
   :node-symbol
   :node-language
   :node-grammar-type
   :node-grammar-symbol
   :node-start-byte
   :node-end-byte
   :node-start-point
   :node-end-point
   :node-string
   :node-text
   :node-null-p
   :node-named-p
   :node-missing-p
   :node-extra-p
   :node-has-changes
   :node-has-error
   :node-error-p
   :node-parent
   :node-child
   :node-child-count
   :node-children
   :node-next-sibling
   :node-prev-sibling
   :node-first-child
   :node-eq
   ;; cursor
   :make-cursor
   :cursor-reset
   :cursor-node
   :cursor-field-name
   :cursor-field-id
   :cursor-goto-parent
   :cursor-goto-next-sibling
   :cursor-goto-prev-sibling
   :cursor-goto-first-child
   :cursor-goto-last-child
   :cursor-goto-descendant
   :cursor-descendant-index
   :cursor-depth
   :cursor-goto-first-child-for-byte
   :cursor-goto-first-child-for-point
   ;; query
   :query
   ;; language
   :include-language
   :make-language
   ))
(in-package :treesitter)

(defmacro defun-doc (name args function &body body)
  "Generate docstrings from wrapped functions."
  (let ((docstring (documentation function 'function)))
    `(defun ,name ,args
       ,docstring
       ,@body)))

(defclass foreign-object ()
  ((pointer
    :initarg :pointer
    :initform (error "Must provide :pointer")
    :accessor pointer)
   (free
    :initarg :free
    :initform #'foreign-free
    :accessor free)))

;; gc dbg counters
(defparameter *alloced* 0)
(defparameter *freed* 0)

(defmethod initialize-instance :after ((self foreign-object) &key &allow-other-keys)
  (incf *alloced*)
  (let ((pointer (slot-value self 'pointer))
        (free (slot-value self 'free)))
    (tg:finalize self (lambda ()
                        (incf *freed*)
                        (funcall free pointer)))))

(defclass language (foreign-object) ())
(defclass parser (foreign-object) ())
(defclass point (foreign-object) ())
(defclass node (foreign-object) ())
(defclass tree (foreign-object) ())
(defclass cursor (foreign-object) ())
(defclass query (foreign-object) ())
(defclass query-cursor (foreign-object) ())

;********************;
;* Section - Parser *;
;********************;

(defun make-parser (&key language timeout cancellation logger)
  (make-instance 'parser
                 :free #'ts-parser-delete
                 :pointer (ts-parser-new :language (pointer language)
                                         :timeout timeout
                                         :cancellation cancellation
                                         :logger logger)))

(defun-doc parser-language (parser) ts-parser-language
  (ts-parser-language (pointer parser)))

(defun-doc (setf parser-language) (value parser) ts-parser-set-language
  (ts-parser-set-language (pointer parser) value))

(defun-doc parser-timeout (parser) ts-parser-timeout-micros
  (ts-parser-timeout-micros (pointer parser)))

(defun-doc (setf parser-timeout) (value parser) ts-parser-set-timeout-micros
  (ts-parser-set-timeout-micros (pointer parser) value))

(defun-doc parser-parse-string (parser value &key (old-tree (null-pointer)) encoding)
    ts-parser-parse-string
  (let ((pointer
          (if encoding
              (ts-parser-parse-string-encoded (pointer parser)
                                              value
                                              encoding
                                              old-tree)
              (ts-parser-parse-string (pointer parser)
                                      value
                                      old-tree))))
    (make-instance 'tree
                   :free #'ts-tree-delete
                   :pointer pointer)))

(defun-doc parser-reset (parser) ts-parser-reset
  (ts-parser-reset (pointer parser)))

;******************;
;* Section - Tree *;
;******************;

(defun-doc tree-root-node (tree &key offset) ts-tree-root-node
  (let ((pointer
          (if offset
              (ts-tree-root-node-with-offset (pointer tree) offset (null-pointer))
              (ts-tree-root-node (pointer tree)))))
    (make-instance 'node
                   :free #'ts-node-delete
                   :pointer pointer)))

(defun tree-language (tree)
  (make-instance 'language
                 :free #'ts-language-delete
                 :pointer (ts-language-copy (ts-tree-language (pointer tree)))))

;******************;
;* Section - Node *;
;******************;

(defun-doc node-type (node) ts-node-type
  (ts-node-type (pointer node)))

(defun-doc node-symbol (node) ts-node-symbol
  (ts-node-symbol (pointer node)))

(defun-doc node-language (node) ts-node-language
  (make-instance 'language
                 :free #'ts-language-delete
                 :pointer (ts-language-copy (ts-node-language (pointer node)))))

(defun-doc node-grammar-type (node) ts-node-grammar-type
  (ts-node-grammar-type (pointer node)))

(defun-doc node-grammar-symbol (node) ts-node-grammar-symbol
  (ts-node-grammar-symbol (pointer node)))

(defun-doc node-start-byte (node) ts-node-start-byte
  (ts-node-start-byte (pointer node)))

(defun-doc node-end-byte (node) ts-node-end-byte
  (ts-node-end-byte (pointer node)))

(defun-doc node-start-point (node) ts-node-start-point
  (let ((point (ts-node-start-point (pointer node))))
    (prog1 (cons (ts-point-row point) (ts-point-column point))
      (ts-point-delete point))))

(defun-doc node-end-point (node) ts-node-end-point
  (let ((point (ts-node-end-point (pointer node))))
    (prog1 (cons (ts-point-row point) (ts-point-column point))
      (ts-point-delete point))))

(defun-doc node-string (node) ts-node-string
  (ts-node-string (pointer node)))

(defmethod node-text (node (source string))
  "Extract the node text from the source."
  (subseq source
          (node-start-byte node)
          (node-end-byte node)))

(defun-doc node-null-p (node) ts-node-is-null
  (ts-node-is-null (pointer node)))

(defun-doc node-named-p (node) ts-node-is-named
  (ts-node-is-named (pointer node)))

(defun-doc node-missing-p (node) ts-node-is-missing
  (ts-node-is-missing (pointer node)))

(defun-doc node-extra-p (node) ts-node-is-extra
  (ts-node-is-extra (pointer node)))

(defun-doc node-has-changes (node) ts-node-has-changes
  (ts-node-has-changes (pointer node)))

(defun-doc node-has-error (node) ts-node-has-error
  (ts-node-has-error (pointer node)))

(defun-doc node-error-p (node) ts-node-is-error
  (ts-node-is-error (pointer node)))

(defun-doc node-parent (node) ts-node-parent
  (make-instance 'node
                 :free #'ts-node-delete
                 :pointer (ts-node-parent (pointer node))))

(defun-doc node-child (node index &key named) ts-node-child
  (make-instance 'node
                 :free #'ts-node-delete
                 :pointer (if named
                              (ts-node-named-child (pointer node) index)
                              (ts-node-child (pointer node) index))))

(defun-doc node-child-count (node &key named) ts-node-child-count
  (if named
      (ts-node-named-child-count (pointer node))
      (ts-node-child-count (pointer node))))

(defun node-children (node &key named)
  "Get all children of a node."
  (loop :for i :from 0 :to (1- (node-child-count node :named named))
        :collect (node-child node i :named named)))

(defun-doc node-next-sibling (node &key named) ts-node-next-sibling
  (make-instance 'node
                 :free #'ts-node-delete
                 :pointer (if named
                              (ts-node-next-named-sibling (pointer node))
                              (ts-node-next-sibling (pointer node)))))

(defun-doc node-prev-sibling (node &key named) ts-node-prev-sibling
  (make-instance 'node
                 :free #'ts-node-delete
                 :pointer (if named
                              (ts-node-prev-named-sibling (pointer node))
                              (ts-node-prev-sibling (pointer node)))))

(defun-doc node-first-child (node byte &key named) ts-node-first-child-for-byte
  (let ((pointer (if named
                     (ts-node-first-named-child-for-byte (pointer node) byte)
                     (ts-node-first-child-for-byte (pointer node) byte))))
    (make-instance 'node
                   :free #'ts-node-delete
                   :pointer pointer)))

(defun-doc node-descendant-for-range (node start end &key named)
    ts-node-descendant-for-byte-range
  (let ((pointer
          (if named
              (ts-node-descendant-for-byte-range (pointer node) start end)
              (ts-node-named-descendant-for-byte-range (pointer node) start end))))
    (make-instance 'node
                   :free #'ts-node-delete
                   :pointer pointer)))

(defun-doc node-eq (node other) ts-node-eq
  (ts-node-eq (pointer node) other))

;************************;
;* Section - TreeCursor *;
;************************;

(defun make-cursor (node)
  (make-instance 'cursor
                 :free #'ts-tree-cursor-delete
                 :pointer (ts-tree-cursor-new (pointer node))))

(defun-doc cursor-reset (cursor node) ts-tree-cursor-reset
  (ts-tree-cursor-reset (pointer cursor) (pointer node)))

(defun-doc cursor-node (cursor) ts-tree-cursor-current-node
  (make-instance 'node
                 :free #'ts-node-delete
                 :pointer (ts-tree-cursor-current-node (pointer cursor))))

(defun-doc cursor-field-name (cursor) ts-tree-cursor-current-field-name
  (ts-tree-cursor-current-field-name (pointer cursor)))

(defun-doc cursor-field-id (cursor) ts-tree-cursor-current-field-id
  (ts-tree-cursor-current-field-id (pointer cursor)))

(defun-doc cursor-goto-parent (cursor) ts-tree-cursor-goto-parent
  (ts-tree-cursor-goto-parent (pointer cursor)))

(defun-doc cursor-goto-next-sibling (cursor) ts-tree-cursor-goto-next-sibling
  (ts-tree-cursor-goto-next-sibling (pointer cursor)))

(defun-doc cursor-goto-prev-sibling (cursor) ts-tree-cursor-goto-previous-sibling
  (ts-tree-cursor-goto-previous-sibling (pointer cursor)))

(defun-doc cursor-goto-first-child (cursor) ts-tree-cursor-goto-first-child
  (ts-tree-cursor-goto-first-child (pointer cursor)))

(defun-doc cursor-goto-last-child (cursor) ts-tree-cursor-goto-last-child
  (ts-tree-cursor-goto-last-child (pointer cursor)))

(defun-doc cursor-goto-descendant (cursor index) ts-tree-cursor-goto-descendant
  (ts-tree-cursor-goto-descendant (pointer cursor) index))

(defun-doc cursor-descendant-index (cursor) ts-tree-cursor-current-descendant-index
  (ts-tree-cursor-current-descendant-index (pointer cursor)))

(defun-doc cursor-depth (cursor) ts-tree-cursor-current-depth
  (ts-tree-cursor-current-depth (pointer cursor)))

(defun-doc cursor-goto-first-child-for-byte (cursor byte) ts-tree-cursor-goto-first-child-for-byte
  (ts-tree-cursor-goto-first-child-for-byte (pointer cursor) byte))

(defun-doc cursor-goto-first-child-for-point (cursor point) ts-tree-cursor-goto-first-child-for-point
  (ts-tree-cursor-goto-first-child-for-point
   (pointer cursor)
   (make-instance 'point
                  :free #'ts-point-delete
                  :pointer (ts-point-new (car point) (cdr point)))))

;*******************/
;* Section - Query */
;*******************/

(defgeneric make-query (language string)
  (:method ((language foreign-object) string)
    (make-query (pointer language) string))
  (:method ((language t) string)
    (make-instance 'query
                   :free #'ts-query-delete
                   :pointer (ts-query-new language string))))

(defun make-query-cursor ()
  (make-instance 'query-cursor
                 :free #'ts-query-cursor-delete
                 :pointer (ts-query-cursor-new)))

(defun-doc query-cursor-exec (query-cursor query node) ts-query-cursor-exec
  (ts-query-cursor-exec (pointer query-cursor)
                        (pointer query)
                        (pointer node)))

(defun for-query-match-captures (qmatch proc)
  (dotimes (i (ts-query-match-capture-count qmatch))
    (let ((capture (ts-query-match-capture qmatch i)))
      (unwind-protect (funcall proc capture)
        (ts-query-capture-delete capture)))))

(defun capture-node (capture)
  (make-instance 'node
                 :free #'ts-node-delete
                 :pointer (ts-query-capture-node capture)))

(defun for-query-cursor-nodes (qcursor proc)
  (flet ((proc-capture (capture)
           (funcall proc (capture-node capture))))
    (loop :with qmatch := (ts-query-match-new)
          :while (ts-query-cursor-next-match (pointer qcursor) qmatch)
          :do (for-query-match-captures qmatch #'proc-capture)
          :finally (ts-query-match-delete qmatch))))

(defun query-cursor-nodes (qcursor)
  (let ((acc))
    (for-query-cursor-nodes qcursor (lambda (node) (push node acc)))
    acc))

(defun ensure-query-valid (query)
  (unless (eq :none (ts-query-error-type (pointer query)))
    (error (format nil "Failure, type: ~a, offset: ~a"
                   (ts-query-error-type (pointer query))
                   (ts-query-error-offset (pointer query))))))

(defun query (node query-string)
  "Query a node with a treesitter query string.
Returns a list of nodes."
  (let ((query (make-query (node-language node) query-string))
        (qcursor (make-query-cursor)))
    (query-cursor-exec qcursor query node)
    (ensure-query-valid query)
    (query-cursor-nodes qcursor)))

;**********************;
;* Section - Language *;
;**********************;

(defvar *languages* (make-hash-table :test #'equal)
  "Language constructors loaded from shared objects.")

(defmacro include-language (lang)
  "Convenience macro to load treesitter language objects.
Interns a function named `tree-sitter-*` that creates a language."
  (let ((fn-symbol (intern (format nil "~:@(tree-sitter-~a~)" lang) :treesitter)))
    `(progn
       (cffi:use-foreign-library ,(format nil "libtree-sitter-~(~a~).so" lang))
       (cffi:defcfun (,(format nil "tree_sitter_~(~a~)" lang) ,fn-symbol) :pointer)
       (setf (gethash ,lang *languages*) (quote ,fn-symbol)))))

(defun make-language (lang)
  (let ((ptr (funcall (gethash lang *languages*))))
    (make-instance 'language
                   :free #'ts-language-delete
                   :pointer ptr)))
