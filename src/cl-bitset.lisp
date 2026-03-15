;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: Apache-2.0

(in-package #:cl-bitset)

;;; All substantive implementations loaded from bitset.lisp
;;; This module re-exports comprehensive bitset operations:
;;; - make-bitset: Create bitset of specified size
;;; - copy-bitset: Clone a bitset
;;; - bitset-set, bitset-clear, bitset-toggle: Bit manipulation
;;; - bitset-test: Check if bit is set
;;; - bitset-and, bitset-or, bitset-xor, bitset-not: Bitwise operations
;;; - bitset-andc1, bitset-andc2: Combined operations
;;; - bitset-count: Count set bits (population count)
;;; - bitset-empty-p: Check if all bits are clear
;;; - bitset-to-list, list-to-bitset: Conversions
;;; - do-bitset, map-bitset: Iteration and mapping

(eval-when (:load-toplevel :compile-toplevel)
  (format t "~&Loading cl-bitset: comprehensive bitset implementation from bitset.lisp~%"))


;;; ============================================================================
;;; Additional Operations (Bitset Size and Utilities)
;;; ============================================================================

;;; Note: bitset-size is already defined in bitset.lisp via the bitset struct accessor

(defun bitset-first-set (bitset)
  "Return index of first set bit, or NIL if empty."
  (declare (type bitset bitset))
  (loop for i from 0 below (bitset-size bitset)
        when (bitset-test bitset i)
          return i))

(defun bitset-next-set (bitset index)
  "Return index of next set bit after INDEX, or NIL if none."
  (declare (type bitset bitset) (type fixnum index))
  (loop for i from (1+ index) below (bitset-size bitset)
        when (bitset-test bitset i)
          return i))

(defun bitset-subset-p (a b)
  "Check if bitset A is a subset of bitset B."
  (declare (type bitset a b))
  (let ((and-result (bitset-and a b)))
    (bitset-equal and-result a)))

(defun bitset-equal (a b)
  "Check if two bitsets are equal."
  (declare (type bitset a b))
  (and (= (bitset-size a) (bitset-size b))
       (equalp (bitset-bits a) (bitset-bits b))))

(defun bitset-disjoint-p (a b)
  "Check if bitsets A and B have no common set bits."
  (declare (type bitset a b))
  (bitset-empty-p (bitset-and a b)))

(defun bitset-complement (bitset &optional result-size)
  "Return complement of BITSET within specified size."
  (declare (type bitset bitset))
  (let ((size (or result-size (bitset-size bitset))))
    (bitset-not bitset (make-bitset size))))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(defun bitset-batch-process (items processor-fn)
  "Applies PROCESSOR-FN to each item in ITEMS, handling errors resiliently.
Returns (values processed-results error-alist)."
  (let ((results nil)
        (errors nil))
    (dolist (item items)
      (handler-case
          (push (funcall processor-fn item) results)
        (error (e)
          (push (cons item e) errors))))
    (values (nreverse results) (nreverse errors))))

(defmacro with-bitset-timing (&body body)
  "Executes BODY and logs the execution time specific to cl-bitset."
  (let ((start (gensym))
        (end (gensym)))
    `(let ((,start (get-internal-real-time)))
       (multiple-value-prog1
           (progn ,@body)
         (let ((,end (get-internal-real-time)))
           (format t "~&[cl-bitset] Execution time: ~A ms~%"
                   (/ (* (- ,end ,start) 1000.0) internal-time-units-per-second)))))))

(defun bitset-health-check ()
  "Performs a basic health check for the cl-bitset module."
  (let ((b (make-bitset 128)))
    (bitset-set b 0)
    (bitset-set b 63)
    (and (bitset-test b 0)
         (bitset-test b 63)
         (= (bitset-count b) 2))))

(defun bitset-from-integer (n)
  "Create bitset from integer representation."
  (let* ((bits (integer-length n))
         (bs (make-bitset (max 64 bits))))
    (dotimes (i bits)
      (when (logbitp i n)
        (bitset-set bs i)))
    bs))

(defun bitset-to-integer (bitset)
  "Convert bitset to integer (up to 64 bits)."
  (let ((result 0))
    (dotimes (i (min 64 (bitset-size bitset)))
      (when (bitset-test bitset i)
        (setf result (logior result (ash 1 i)))))
    result))

(defun bitset-popcount (bitset)
  "Alias for bitset-count (population count)."
  (bitset-count bitset))

;;; ============================================================================
;;; Advanced Bitset Operations
;;; ============================================================================

(defun bitset-hamming-distance (bitset1 bitset2)
  "Compute Hamming distance between two bitsets."
  (bitset-count (bitset-xor bitset1 bitset2)))

(defun bitset-jaccard-similarity (bitset1 bitset2)
  "Compute Jaccard similarity between two bitsets: |A ∩ B| / |A ∪ B|."
  (let ((intersection (bitset-count (bitset-and bitset1 bitset2)))
        (union (bitset-count (bitset-or bitset1 bitset2))))
    (if (zerop union)
        1.0
        (float (/ intersection union)))))

(defun bitset-is-superset-p (a b)
  "Check if bitset A is a superset of bitset B."
  (let ((and-result (bitset-and a b)))
    (bitset-equal and-result b)))

(defun bitset-is-proper-subset-p (a b)
  "Check if bitset A is a proper subset of bitset B (A ⊂ B, A ≠ B)."
  (and (bitset-subset-p a b)
       (not (bitset-equal a b))))

(defun bitset-is-proper-superset-p (a b)
  "Check if bitset A is a proper superset of bitset B (A ⊃ B, A ≠ B)."
  (and (bitset-is-superset-p a b)
       (not (bitset-equal a b))))

(defun bitset-symmetric-difference (a b &optional result)
  "Return bitwise symmetric difference (A ⊕ B) of bitsets A and B."
  (bitset-xor a b result))

(defun bitset-fill (bitset)
  "Set all bits in bitset to 1."
  (dotimes (i (bitset-size bitset) bitset)
    (bitset-set bitset i)))

(defun bitset-clear-all (bitset)
  "Clear all bits in bitset to 0."
  (dotimes (i (bitset-size bitset) bitset)
    (bitset-clear bitset i)))

(defun bitset-flip (bitset)
  "Flip (toggle) all bits in bitset."
  (dotimes (i (bitset-size bitset) bitset)
    (bitset-toggle bitset i)))

(defun bitset-any-p (bitset)
  "Return T if any bit is set."
  (not (bitset-empty-p bitset)))

(defun bitset-all-p (bitset)
  "Return T if all bits are set."
  (= (bitset-count bitset) (bitset-size bitset)))

(defun bitset-none-p (bitset)
  "Return T if no bits are set."
  (bitset-empty-p bitset))

(defun bitset-leading-zeros (bitset)
  "Count number of leading zeros (from high bit down)."
  (let ((size (bitset-size bitset)))
    (loop for i from (1- size) downto 0
          count (not (bitset-test bitset i))
          until (bitset-test bitset i))))

(defun bitset-trailing-zeros (bitset)
  "Count number of trailing zeros (from low bit up)."
  (loop for i from 0 below (bitset-size bitset)
        count (not (bitset-test bitset i))
        until (bitset-test bitset i)))

(defun bitset-msb-position (bitset)
  "Find position of most significant bit (highest set bit), or -1 if empty."
  (loop for i from (1- (bitset-size bitset)) downto 0
        when (bitset-test bitset i)
          return i
        finally (return -1)))

(defun bitset-lsb-position (bitset)
  "Find position of least significant bit (lowest set bit), or -1 if empty."
  (loop for i from 0 below (bitset-size bitset)
        when (bitset-test bitset i)
          return i
        finally (return -1)))

(defun bitset-rotate-left (bitset n)
  "Rotate bitset left by N positions, returning new bitset."
  (let* ((size (bitset-size bitset))
         (n (mod n size))
         (result (make-bitset size)))
    (dotimes (i size result)
      (when (bitset-test bitset i)
        (bitset-set result (mod (+ i n) size))))))

(defun bitset-rotate-right (bitset n)
  "Rotate bitset right by N positions, returning new bitset."
  (let* ((size (bitset-size bitset))
         (n (mod n size))
         (result (make-bitset size)))
    (dotimes (i size result)
      (when (bitset-test bitset i)
        (bitset-set result (mod (- i n) size))))))

(defun bitset-shift-left (bitset n &optional result-size)
  "Shift bitset left by N positions."
  (let* ((size (bitset-size bitset))
         (new-size (or result-size size))
         (result (make-bitset new-size)))
    (dotimes (i size result)
      (when (bitset-test bitset i)
        (let ((new-pos (+ i n)))
          (when (< new-pos new-size)
            (bitset-set result new-pos)))))))

(defun bitset-shift-right (bitset n &optional result-size)
  "Shift bitset right by N positions."
  (let* ((size (bitset-size bitset))
         (new-size (or result-size size))
         (result (make-bitset new-size)))
    (dotimes (i size result)
      (when (bitset-test bitset i)
        (let ((new-pos (- i n)))
          (when (>= new-pos 0)
            (bitset-set result new-pos)))))))

(defun bitset-reverse (bitset)
  "Reverse all bits in bitset."
  (let* ((size (bitset-size bitset))
         (result (make-bitset size)))
    (dotimes (i size result)
      (when (bitset-test bitset i)
        (bitset-set result (- size 1 i))))))

(defun bitset-from-bytes (bytes)
  "Create bitset from byte vector (LSB first)."
  (let* ((nbits (* 8 (length bytes)))
         (bs (make-bitset nbits)))
    (dotimes (byte-idx (length bytes) bs)
      (let ((byte-val (aref bytes byte-idx)))
        (dotimes (bit-idx 8)
          (when (logbitp bit-idx byte-val)
            (bitset-set bs (+ (* byte-idx 8) bit-idx))))))))

(defun bitset-to-bytes (bitset)
  "Convert bitset to byte vector (LSB first)."
  (let* ((nbytes (ceiling (bitset-size bitset) 8))
         (result (make-array nbytes :element-type '(unsigned-byte 8)
                                    :initial-element 0)))
    (dotimes (i (bitset-size bitset) result)
      (when (bitset-test bitset i)
        (let ((byte-idx (ash i -3))
              (bit-idx (logand i #x7)))
          (setf (aref result byte-idx)
                (logior (aref result byte-idx) (ash 1 bit-idx))))))))

(defun bitset-from-hex (hex-string)
  "Create bitset from hexadecimal string."
  (let* ((ndigits (length hex-string))
         (nbits (* 4 ndigits))
         (bs (make-bitset nbits)))
    (dotimes (i ndigits bs)
      (let ((digit (parse-integer hex-string :start i :end (1+ i) :radix 16)))
        (dotimes (bit 4)
          (when (logbitp bit digit)
            (bitset-set bs (+ (* (- ndigits 1 i) 4) bit))))))))

(defun bitset-to-hex (bitset)
  "Convert bitset to hexadecimal string."
  (let* ((bytes (bitset-to-bytes bitset))
         (result (make-string (* 2 (length bytes)))))
    (loop for i from 0 below (length bytes)
          for j from 0 by 2
          do (let ((b (aref bytes i)))
               (setf (aref result j) (digit-char (ash b -4) 16))
               (setf (aref result (1+ j)) (digit-char (logand b #xF) 16))))
    (nreverse result)))

(defun bitset-from-binary (binary-string)
  "Create bitset from binary string ('0' and '1' characters)."
  (let ((size (length binary-string))
        (bs (make-bitset (length binary-string))))
    (dotimes (i size bs)
      (when (char= (aref binary-string (- size 1 i)) #\1)
        (bitset-set bs i)))))

(defun bitset-to-binary (bitset)
  "Convert bitset to binary string."
  (let ((result (make-string (bitset-size bitset) :initial-element #\0)))
    (dotimes (i (bitset-size bitset) result)
      (when (bitset-test bitset i)
        (setf (aref result (- (bitset-size bitset) 1 i)) #\1)))))

(defun bitset-cardinality (bitset)
  "Alias for bitset-count - return number of set bits."
  (bitset-count bitset))

(defun bitset-density (bitset)
  "Return fraction of set bits (0.0 to 1.0)."
  (float (/ (bitset-count bitset) (bitset-size bitset))))