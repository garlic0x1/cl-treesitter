(ql:quickload :cffi-toolchain)

(defclass c-source-file (asdf:source-file) ())

(defmethod asdf:perform ((op asdf:load-op) (obj c-source-file)) t)

(defmethod asdf:perform ((op asdf:compile-op) (obj c-source-file))
  (with-slots ((name asdf/component:absolute-pathname)) obj
    (cffi-toolchain:link-shared-library
     (format nil "~a.so" name)
     (list (format nil "~a.c" name)))))

(asdf:defsystem #:cl-treesitter
  :author "garlic0x1"
  :license "MIT"
  :depends-on (:cffi :trivial-garbage)
  :components ((:c-source-file "shim")
               (:file "bindings")
               (:file "treesitter")))

(asdf:defsystem #:cl-treesitter/test
  :depends-on (:alexandria :fiveam :cl-treesitter)
  :components ((:module "t"
                :components ((:file "test-bindings")
                             (:file "test-treesitter")))))
