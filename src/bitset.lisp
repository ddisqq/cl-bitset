;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: Apache-2.0

;;;; bitset.lisp
;;;; Efficient bitset operations implementation

(in-package #:cl-bitset)

;;; Bitset Type

(defstruct (bitset (:constructor %make-bitset)
                    (:copier nil))  ; We provide our own copy-bitset
  "An efficient bitset backed by a bit-vector."
  (bits #* :type simple-bit-vector)
  (size 0 :type fixnum))

(defun make-bitset (size &key initial-element)
  "Create a new bitset capable of holding SIZE bits.
   If INITIAL-ELEMENT is 1, all bits are set; otherwise all cleared."
  (declare (type fixnum size))
  (%make-bitset :bits (make-array size :element-type 'bit
                                        :initial-element (if initial-element 1 0))
                :size size))

(defun copy-bitset (bitset)
  "Create a copy of BITSET."
  (%make-bitset :bits (copy-seq (bitset-bits bitset))
                :size (bitset-size bitset)))

;;; Bit Manipulation

(defun bitset-set (bitset index)
  "Set the bit at INDEX in BITSET. Returns BITSET."
  (declare (type bitset bitset)
           (type fixnum index)
           (optimize (speed 3) (safety 1)))
  (setf (sbit (bitset-bits bitset) index) 1)
  bitset)

(defun bitset-clear (bitset index)
  "Clear the bit at INDEX in BITSET. Returns BITSET."
  (declare (type bitset bitset)
           (type fixnum index)
           (optimize (speed 3) (safety 1)))
  (setf (sbit (bitset-bits bitset) index) 0)
  bitset)

(defun bitset-test (bitset index)
  "Return T if the bit at INDEX in BITSET is set, NIL otherwise."
  (declare (type bitset bitset)
           (type fixnum index)
           (optimize (speed 3) (safety 1)))
  (= 1 (sbit (bitset-bits bitset) index)))

(defun bitset-toggle (bitset index)
  "Toggle the bit at INDEX in BITSET. Returns BITSET."
  (declare (type bitset bitset)
           (type fixnum index)
           (optimize (speed 3) (safety 1)))
  (let ((bits (bitset-bits bitset)))
    (setf (sbit bits index) (if (zerop (sbit bits index)) 1 0)))
  bitset)

;;; Bitwise Operations

(defun ensure-same-size (a b operation)
  "Ensure bitsets A and B have the same size for OPERATION."
  (unless (= (bitset-size a) (bitset-size b))
    (error "Bitsets must have same size for ~A: ~D vs ~D"
           operation (bitset-size a) (bitset-size b))))

(defun bitset-and (a b &optional result)
  "Return the bitwise AND of bitsets A and B."
  (ensure-same-size a b 'bitset-and)
  (let ((result (or result (make-bitset (bitset-size a)))))
    (setf (bitset-bits result)
          (bit-and (bitset-bits a) (bitset-bits b) (bitset-bits result)))
    result))

(defun bitset-or (a b &optional result)
  "Return the bitwise OR of bitsets A and B."
  (ensure-same-size a b 'bitset-or)
  (let ((result (or result (make-bitset (bitset-size a)))))
    (setf (bitset-bits result)
          (bit-ior (bitset-bits a) (bitset-bits b) (bitset-bits result)))
    result))

(defun bitset-xor (a b &optional result)
  "Return the bitwise XOR of bitsets A and B."
  (ensure-same-size a b 'bitset-xor)
  (let ((result (or result (make-bitset (bitset-size a)))))
    (setf (bitset-bits result)
          (bit-xor (bitset-bits a) (bitset-bits b) (bitset-bits result)))
    result))

(defun bitset-not (bitset &optional result)
  "Return the bitwise NOT of BITSET."
  (let ((result (or result (make-bitset (bitset-size bitset)))))
    (setf (bitset-bits result)
          (bit-not (bitset-bits bitset) (bitset-bits result)))
    result))

(defun bitset-andc1 (a b &optional result)
  "Return the bitwise AND of (NOT A) and B."
  (ensure-same-size a b 'bitset-andc1)
  (let ((result (or result (make-bitset (bitset-size a)))))
    (setf (bitset-bits result)
          (bit-andc1 (bitset-bits a) (bitset-bits b) (bitset-bits result)))
    result))

(defun bitset-andc2 (a b &optional result)
  "Return the bitwise AND of A and (NOT B)."
  (ensure-same-size a b 'bitset-andc2)
  (let ((result (or result (make-bitset (bitset-size a)))))
    (setf (bitset-bits result)
          (bit-andc2 (bitset-bits a) (bitset-bits b) (bitset-bits result)))
    result))

;;; Counting and Enumeration

(defun bitset-count (bitset)
  "Return the number of set bits in BITSET."
  (declare (type bitset bitset)
           (optimize (speed 3) (safety 1)))
  (count 1 (bitset-bits bitset)))

(defun bitset-empty-p (bitset)
  "Return T if no bits are set in BITSET."
  (zerop (bitset-count bitset)))

;;; Conversion

(defun bitset-to-list (bitset)
  "Return a list of indices where bits are set."
  (declare (type bitset bitset))
  (loop for i from 0 below (bitset-size bitset)
        when (bitset-test bitset i)
          collect i))

(defun list-to-bitset (list &optional size)
  "Create a bitset from a LIST of indices.
   SIZE defaults to one more than the maximum index."
  (let* ((max-index (if list (reduce #'max list) 0))
         (size (or size (1+ max-index)))
         (bitset (make-bitset size)))
    (dolist (index list)
      (bitset-set bitset index))
    bitset))

;;; Iteration

(defmacro do-bitset ((var bitset &optional result) &body body)
  "Iterate over set bit indices in BITSET, binding each to VAR."
  (let ((bits (gensym "BITS"))
        (i (gensym "I"))
        (bs (gensym "BITSET")))
    `(let* ((,bs ,bitset)
            (,bits (bitset-bits ,bs)))
       (dotimes (,i (bitset-size ,bs) ,result)
         (when (= 1 (sbit ,bits ,i))
           (let ((,var ,i))
             ,@body))))))

(defun map-bitset (function bitset)
  "Apply FUNCTION to each set bit index in BITSET, collecting results."
  (let ((results '()))
    (do-bitset (i bitset (nreverse results))
      (push (funcall function i) results))))

;;; ============================================================================
;;; Advanced Bitset Utilities
;;; ============================================================================

(defun bitset-find-first-set (bitset)
  "Find index of first set bit, return NIL if none."
  (do-bitset (i bitset nil)
    (return i)))

(defun bitset-find-last-set (bitset)
  "Find index of last set bit, return NIL if none."
  (let ((last nil))
    (do-bitset (i bitset last)
      (setf last i))))

(defun bitset-clear-range (bitset start end)
  "Clear bits in range [START, END)."
  (loop for i from start below (min end (bitset-size bitset))
        do (bitset-clear bitset i))
  bitset)

(defun bitset-set-range (bitset start end)
  "Set bits in range [START, END)."
  (loop for i from start below (min end (bitset-size bitset))
        do (bitset-set bitset i))
  bitset)

(defun bitset-toggle-range (bitset start end)
  "Toggle bits in range [START, END)."
  (loop for i from start below (min end (bitset-size bitset))
        do (bitset-toggle bitset i))
  bitset)

(defun bitset-count-range (bitset start end)
  "Count set bits in range [START, END)."
  (loop for i from start below (min end (bitset-size bitset))
        count (bitset-test bitset i)))

(defun bitset-any-in-range-p (bitset start end)
  "Return T if any bit is set in range [START, END)."
  (loop for i from start below (min end (bitset-size bitset))
        thereis (bitset-test bitset i)))

(defun bitset-all-in-range-p (bitset start end)
  "Return T if all bits are set in range [START, END)."
  (loop for i from start below (min end (bitset-size bitset))
        always (bitset-test bitset i)))

(defun bitset-next-set-bit (bitset index)
  "Find next set bit starting after INDEX, return NIL if none."
  (loop for i from (1+ index) below (bitset-size bitset)
        when (bitset-test bitset i)
          return i))

(defun bitset-prev-set-bit (bitset index)
  "Find previous set bit before INDEX, return NIL if none."
  (loop for i from (1- index) downto 0
        when (bitset-test bitset i)
          return i))

(defun bitset-nth-set-bit (bitset n)
  "Find the Nth set bit (0-indexed), return NIL if fewer than N+1 bits set."
  (let ((count 0))
    (do-bitset (i bitset nil)
      (when (= count n) (return i))
      (incf count))))

(defun bitset-intersect (bitsets)
  "Return intersection of multiple bitsets."
  (when (null bitsets)
    (error "Cannot intersect empty list of bitsets"))
  (let ((result (copy-bitset (first bitsets))))
    (dolist (bs (rest bitsets) result)
      (setf (bitset-bits result)
            (bit-and (bitset-bits result)
                     (bitset-bits bs)
                     (bitset-bits result))))))

(defun bitset-union (bitsets)
  "Return union of multiple bitsets."
  (when (null bitsets)
    (error "Cannot union empty list of bitsets"))
  (let ((result (copy-bitset (first bitsets))))
    (dolist (bs (rest bitsets) result)
      (setf (bitset-bits result)
            (bit-ior (bitset-bits result)
                     (bitset-bits bs)
                     (bitset-bits result))))))

(defun bitset-difference (a b)
  "Return difference A - B (bits in A but not in B)."
  (bitset-andc2 a b))

(defun bitset-apply (bitset function &key (start 0) (end nil))
  "Apply function to each bit in range, modifying bitset."
  (let ((end-idx (or end (bitset-size bitset))))
    (loop for i from start below (min end-idx (bitset-size bitset))
          do (let ((bit-val (if (bitset-test bitset i) 1 0)))
               (if (funcall function bit-val)
                   (bitset-set bitset i)
                   (bitset-clear bitset i))))
    bitset))

(defun bitset-fold (function bitset &optional initial)
  "Fold function over set bit indices."
  (let ((acc initial))
    (do-bitset (i bitset acc)
      (setf acc (funcall function acc i)))))

(defun bitset-every-p (predicate bitset)
  "Return T if predicate is true for every set bit."
  (not (do-bitset (i bitset nil)
         (unless (funcall predicate i) (return t)))))

(defun bitset-some-p (predicate bitset)
  "Return T if predicate is true for some set bit."
  (do-bitset (i bitset nil)
    (when (funcall predicate i) (return t))))

(defun bitset-find (predicate bitset)
  "Find first set bit that satisfies predicate, return NIL if none."
  (do-bitset (i bitset nil)
    (when (funcall predicate i) (return i))))

(defun bitset-count-if (predicate bitset)
  "Count set bits that satisfy predicate."
  (let ((count 0))
    (do-bitset (i bitset count)
      (when (funcall predicate i) (incf count)))))

(defun bitset-fill-range (bitset start end value)
  "Fill range [START, END) with bit VALUE (0 or 1)."
  (if (zerop value)
      (bitset-clear-range bitset start end)
      (bitset-set-range bitset start end)))
