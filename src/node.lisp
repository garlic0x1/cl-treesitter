(defpackage :treesitter/node
  (:use :cl)
  (:import-from :treesitter/ffi
                :ts-node-type
                :ts-node-symbol
                :ts-node-language
                :ts-node-grammar-type
                :ts-node-grammar-symbol
                :ts-node-start-byte
                :ts-node-end-byte
                :ts-node-start-point
                :ts-node-end-point
                :ts-node-string
                :ts-node-is-null
                :ts-node-is-named
                :ts-node-is-extra
                :ts-node-has-changes
                :ts-node-has-error
                :ts-node-is-error
                :ts-node-parse-state
                :ts-node-next-parse-state)
  (:export :ts-node-type
           :ts-node-symbol
           :ts-node-language
           :ts-node-grammar-type
           :ts-node-grammar-symbol
           :ts-node-start-byte
           :ts-node-end-byte
           :ts-node-start-point
           :ts-node-end-point
           :ts-node-string
           :ts-node-is-null
           :ts-node-is-named
           :ts-node-is-extra
           :ts-node-has-changes
           :ts-node-has-error
           :ts-node-is-error
           :ts-node-parse-state
           :ts-node-next-parse-state))
(in-package :treesitter/node)
