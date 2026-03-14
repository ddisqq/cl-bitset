;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: BSD-3-Clause

;;;; cl-bitset.asd
;;;; Efficient bitset operations with zero external dependencies

(asdf:defsystem #:cl-bitset
  :description "Pure Common Lisp efficient bitset operations library"
  :author "Parkian Company LLC"
  :license "BSD-3-Clause"
  :version "0.1.0"
  :serial t
  :components ((:file "package")
               (:module "src"
                :serial t
                :components ((:file "bitset")))))

(asdf:defsystem #:cl-bitset/test
  :description "Tests for cl-bitset"
  :depends-on (#:cl-bitset)
  :serial t
  :components ((:module "test"
                :components ((:file "test-bitset"))))
  :perform (asdf:test-op (o c)
             (let ((result (uiop:symbol-call :cl-bitset.test :run-tests)))
               (unless result
                 (error "Tests failed")))))
