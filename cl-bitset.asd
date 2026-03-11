;;;; cl-bitset.asd
;;;; Efficient bitset operations with zero external dependencies

(asdf:defsystem #:cl-bitset
  :description "Pure Common Lisp efficient bitset operations library"
  :author "Parkian Company LLC"
  :license "BSD-3-Clause"
  :version "1.0.0"
  :serial t
  :components ((:file "package")
               (:module "src"
                :serial t
                :components ((:file "bitset")))))
