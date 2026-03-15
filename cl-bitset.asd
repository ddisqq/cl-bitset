;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: BSD-3-Clause

;;;; cl-bitset.asd
;;;; Efficient bitset operations with zero external dependencies

(asdf:defsystem #:cl-bitset
  :description "Pure Common Lisp efficient bitset operations library"
  :author "Park Ian Co"
  :license "Apache-2.0"
  :version "0.1.0"
  :serial t
  :components ((:file "src/package")
               (:module "src"
                :components ((:file "conditions" :depends-on ("package"))
                             (:file "types" :depends-on ("package"))
                             (:file "bitset" :depends-on ("package" "types"))
                             (:file "algorithms" :depends-on ("bitset"))
                             (:file "cl-bitset" :depends-on ("package" "conditions" "types" "bitset" "algorithms")))))))

(asdf:defsystem #:cl-bitset/test
  :description "Tests for cl-bitset"
  :depends-on (#:cl-bitset)
  :serial t
  :components ((:module "test"
                :components ((:file "test-bitset")))))
