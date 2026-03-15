;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: Apache-2.0

(in-package #:cl-bitset)

;;; Core types for cl-bitset
(deftype cl-bitset-id () '(unsigned-byte 64))
(deftype cl-bitset-status () '(member :ready :active :error :shutdown))
