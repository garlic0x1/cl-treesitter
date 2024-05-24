(defpackage :treesitter/parser
  (:use :cl)
  (:import-from :treesitter/ffi
                :ts-parser-language
                :ts-parser-set-language
                :ts-parser-timeout-micros
                :ts-parser-set-timeout-micros
                :ts-parser-cancellation-flag
                :ts-parser-set-cancellation-flag)
  (:export :ts-parser-parse-string
           :ts-parser-language
           :ts-parser-timeout-micros
           :ts-parser-cancellation-flag))
(in-package :treesitter/parser)

(defun (setf ts-parser-language) (value parser)
  (ts-parser-set-language parser value))

(defun (setf ts-parser-timeout-micros) (value parser)
  (ts-parser-set-timeout-micros parser value))

(defun (setf ts-parser-cancellation-flag) (value parser)
  (ts-parser-set-cancellation-flag parser value))

(defun ts-parser-parse-string (parser string &key (old-tree (cffi:null-pointer))
                                                  encoding)
  (if encoding
      (ts-parser-parse-string-encoded parser old-tree string (length string) encoding)
      (ts-parser-parse-string* parser old-tree string (length string))))
