(asdf:defsystem "cl-treesitter"
  :author "garlic0x1"
  :license "MIT"
  :depends-on (:cffi :cffi-libffi)
  :components ((:module "src"
                :components ((:file "package")
                             (:file "types")
                             (:file "parser")
                             (:file "language")
                             (:file "tree")
                             (:file "node")
                             (:file "cursor")))))
