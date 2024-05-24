(defmethod asdf:perform ((op asdf:load-op) (obj asdf:c-source-file)) t)

(defmethod asdf:perform ((op asdf:compile-op) (obj asdf:c-source-file))
  (uiop:with-current-directory ((asdf:system-relative-pathname :treesitter ""))
    (with-slots ((name asdf/component:name)) obj
      (uiop:run-program
       (format nil "gcc -ltree-sitter -shared -o ~a.so ~a.c" name name)))))

(asdf:defsystem "treesitter"
  :author "garlic0x1"
  :license "MIT"
  :depends-on (:cffi)
  :components ((:c-source-file "shim")
               (:file "treesitter")))
