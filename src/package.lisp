;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: Apache-2.0

;;;; package.lisp
;;;; Package definition for cl-bitset

(defpackage #:cl-bitset
  (:use #:cl)
  (:export
   #:identity-list
   #:flatten
   #:map-keys
   #:now-timestamp
#:with-bitset-timing
   #:bitset-batch-process
   #:bitset-health-check;; Bitset type and constructor
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
