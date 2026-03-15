;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: Apache-2.0

(in-package #:cl-bitset)

(define-condition cl-bitset-error (error)
  ((message :initarg :message :reader cl-bitset-error-message))
  (:report (lambda (condition stream)
             (format stream "cl-bitset error: ~A" (cl-bitset-error-message condition)))))
