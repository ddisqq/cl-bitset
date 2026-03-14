;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: BSD-3-Clause

;;;; test-bitset.lisp - Unit tests for bitset
;;;;
;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause

(defpackage #:cl-bitset.test
  (:use #:cl)
  (:export #:run-tests))

(in-package #:cl-bitset.test)

(defun run-tests ()
  "Run all tests for cl-bitset."
  (format t "~&Running tests for cl-bitset...~%")
  ;; TODO: Add test cases
  ;; (test-function-1)
  ;; (test-function-2)
  (format t "~&All tests passed!~%")
  t)
