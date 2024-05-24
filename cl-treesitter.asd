(asdf:defsystem "cl-treesitter"
  :author "garlic0x1"
  :license "MIT"
  :depends-on (:cffi :cffi-libffi :trivial-garbage)
  :components ((:module "src"
                :components ((:file "bindings")
                             (:file "package")
                             (:file "types")
                             (:file "language")))))
