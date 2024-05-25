(ql:quickload :cffi-toolchain)

(defclass c-source-file (asdf:source-file) ())

(defmethod asdf:perform ((op asdf:load-op) (obj c-source-file)) t)

(defmethod asdf:perform ((op asdf:compile-op) (obj c-source-file))
  (with-slots ((name asdf/component:absolute-pathname)) obj
    (cffi-toolchain:link-shared-library
     (format nil "~a.so" name)
     (list (format nil "~a.c" name)))))

(asdf:defsystem "treesitter"
  :author "garlic0x1"
  :license "MIT"
  :depends-on (:cffi)
  :components ((:c-source-file "shim")
               (:file "treesitter")))

(asdf:defsystem "treesitter/test"
  :depends-on (:fiveam :treesitter)
  :components ((:module "t"
                :components ((:file "test")))))
