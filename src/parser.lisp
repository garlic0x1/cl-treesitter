(in-package :treesitter)

(defcfun ("ts_parser_new" ts-parser-new!) :pointer)

(defcfun "ts_parser_delete" :void
  (parser :pointer))

(defmacro with-ts-parser ((parser lang-string) &body body)
  `(let ((,parser (ts-parser-new!)))
     (unwind-protect (with-ts-language (lang ,lang-string)
                       (setf (ts-parser-language ,parser) lang)
                       ,@body)
       (ts-parser-delete ,parser))))

(defcfun "ts_parser_language" :pointer
  (parser :pointer))

(defcfun "ts_parser_set_language" :bool
  (parser :pointer)
  (language :pointer))

(defun (setf ts-parser-language) (value parser)
  (ts-parser-set-language parser value))

(defcfun "ts_parser_included_ranges" :pointer
  (parser :pointer)
  (count (:pointer :uint32)))

(defcfun "ts_parser_set_included_ranges" :bool
  (parser :pointer)
  (ranges :pointer)
  (count :uint32))

(defcfun "ts_parser_parse" :pointer
  (parser :pointer)
  (old-tree :pointer)
  (input (:struct ts-input)))

(defcfun ("ts_parser_parse_string" ts-parser-parse-string*) :pointer
  (parser :pointer)
  (old-tree :pointer)
  (string :string)
  (length :uint32))

(defcfun "ts_parser_parse_string_encoded" :pointer
  (parser :pointer)
  (old-tree :pointer)
  (string :string)
  (length :uint32)
  (encoding :int))

(defun ts-parser-parse-string (parser string &key (old-tree (cffi:null-pointer))
                                                  encoding)
  (if encoding
      (ts-parser-parse-string-encoded parser old-tree string (length string) encoding)
      (ts-parser-parse-string* parser old-tree string (length string))))

(defmacro with-ts-tree ((tree parser string &key (old-tree (cffi:null-pointer))
                                                 encoding)
                        &body body)
  `(let ((,tree (ts-parser-parse-string ,parser ,string
                                        :old-tree ,old-tree
                                        :encoding ,encoding)))
     (unwind-protect (progn ,@body)
       (ts-tree-delete ,tree))))

(defcfun "ts_parser_reset" :void
  (parser :pointer))

(defcfun "ts_parser_timeout_micros" :uint64
  (parser :pointer))

(defcfun "ts_parser_set_timeout_micros" :void
  (parser :pointer)
  (timeout-micros :uint64))

(defun (setf ts-parser-timeout-micros) (value parser)
  (ts-parser-set-timeout-micros parser value))

(defcfun "ts_parser_cancellation_flag" :int
  (parser :pointer))

(defcfun "ts_parser_set_cancellation_flag" :void
  (parser :pointer)
  (flag :pointer))

(defun (setf ts-parser-cancellation-flag) (value parser)
  (ts-parser-set-cancellation-flag parser value))

(defcfun "ts_parser_logger" (:struct ts-logger)
  (parser :pointer))

(defcfun "ts_parser_print_dot_graphs" :void
  (parser :pointer)
  (fd :int))
