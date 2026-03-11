;;;; package.lisp
;;;; Package definition for cl-bitset

(defpackage #:cl-bitset
  (:use #:cl)
  (:export
   ;; Bitset type and constructor
   #:bitset
   #:make-bitset
   #:copy-bitset
   ;; Bit manipulation
   #:bitset-set
   #:bitset-clear
   #:bitset-test
   #:bitset-toggle
   ;; Bitwise operations
   #:bitset-and
   #:bitset-or
   #:bitset-xor
   #:bitset-not
   #:bitset-andc1
   #:bitset-andc2
   ;; Counting and enumeration
   #:bitset-count
   #:bitset-empty-p
   #:bitset-size
   ;; Conversion
   #:bitset-to-list
   #:list-to-bitset
   ;; Iteration
   #:do-bitset
   #:map-bitset))
