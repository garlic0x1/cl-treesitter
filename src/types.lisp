(defpackage :treesitter/types
  (:use :cl)
  (:export :foreign-object
           :foreign-object-pointer
           :foreign-object-free
           :foreign-object-gc
           :ts-language
           :ts-parser
           :ts-tree
           :ts-node
           :ts-cursor))
(in-package :treesitter)

(defclass foreign-object ()
  ((pointer
    :initarg :pointer
    :initform (error "Must provide :pointer")
    :accessor foreign-object-pointer)
   (free
    :initarg :free
    :initform (error "Must provide :free")
    :accessor foreign-object-free)
   (gc
    :initarg :gc
    :initform t
    :accessor foreign-object-gc)))

(defmethod initialize-instance :after ((self foreign-object) &key &allow-other-keys)
  (when (foreign-object-gc self)
    (let ((pointer (foreign-object-pointer self))
          (free (foreign-object-free self)))
      (unless pointer (error ":pointer is nil."))
      (unless free (error ":free is nil."))
      (tg:finalize self (lambda () (funcall free pointer))))))

(defclass ts-language (foreign-object) ())
(defclass ts-parser (foreign-object) ())
(defclass ts-tree (foreign-object) ())
(defclass ts-node (foreign-object) ())
(defclass ts-cursor (foreign-object) ())
