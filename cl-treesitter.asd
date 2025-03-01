(or
 (when (find-package '#:OCICL-RUNTIME)
   (progv (list (find-symbol "*DOWNLOAD*" '#:OCICL-RUNTIME)
                (find-symbol "*VERBOSE*" '#:OCICL-RUNTIME))
       (list t t)
     (funcall (find-symbol "LOAD-SYSTEM" '#:asdf) :cffi-toolchain)))
 (when (find-package '#:QUICKLISP)
   (funcall (find-symbol "QUICKLOAD" '#:QUICKLISP) :cffi-toolchain))
 (when (find-package '#:ASDF)
   (funcall (find-symbol "LOAD-SYSTEM" '#:ASDF) :cffi-toolchain))
 (error "Unable to find any system-loading mechanism."))

(defparameter *ld-dll-flags*
  #+darwin '("-I/opt/homebrew/include" "-L/opt/homebrew/lib" "-ltree-sitter")
  #-darwin '())

(defclass c-source-file (asdf:source-file) ())

(defmethod asdf:perform ((op asdf:load-op) (obj c-source-file)) t)

(defmethod asdf:perform ((op asdf:compile-op) (obj c-source-file))
  (with-slots ((name asdf/component:absolute-pathname)) obj
    (let ((cffi-toolchain:*ld-dll-flags*
            (append cffi-toolchain:*ld-dll-flags* *ld-dll-flags*))) 
      (cffi-toolchain:link-shared-library
       #+darwin (format nil "~a.dylib" name)
       #-darwin (format nil "~a.so" name)
       (list (format nil "~a.c" name))))))

(asdf:defsystem #:cl-treesitter
  :author "garlic0x1"
  :license "MIT"
  :description "libtree-sitter bindings for Common Lisp"
  :depends-on (:cffi :trivial-garbage)
  :components ((:c-source-file "shim")
               (:file "bindings")
               (:file "treesitter")))

(asdf:defsystem #:cl-treesitter/test
  :description "Tests for cl-treesitter"
  :depends-on (:alexandria :fiveam :cl-treesitter)
  :components ((:module "t"
                :components ((:file "test-bindings")
                             (:file "test-treesitter")))))
