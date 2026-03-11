;;;; bitset.lisp
;;;; Efficient bitset operations implementation

(in-package #:cl-bitset)

;;; Bitset Type

(defstruct (bitset (:constructor %make-bitset))
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
