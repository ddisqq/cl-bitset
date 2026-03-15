;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; algorithms.lisp - Advanced bitset algorithms and operations
;;;; Comprehensive algorithms for bit manipulation, analysis, and processing

(in-package #:cl-bitset)

;;; ============================================================================
;;; Bit Scanning and Enumeration Algorithms
;;; ============================================================================

(defun bitset-enumerate-subsets (bitset callback)
  "Enumerate all subsets of BITSET by calling CALLBACK with each subset."
  (let ((state 0)
        (mask (bitset-to-integer bitset)))
    (loop do
      (funcall callback state)
      (setf state (logand (1- state) mask))
      (when (zerop state) (return)))))

(defun bitset-generate-combinations (bitset k callback)
  "Generate all combinations of K bits from BITSET."
  (let* ((set-bits (bitset-to-list bitset))
         (n (length set-bits)))
    (when (<= k n)
      (labels ((generate (start result)
                 (cond
                   ((= (length result) k)
                    (funcall callback result))
                   ((> start n) nil)
                   (t
                    (generate (1+ start) result)
                    (generate (1+ start) (append result (list (nth start set-bits))))))))
        (generate 0 nil)))))

(defun bitset-popcount-fast (bitset)
  "Fast population count using Brian Kernighan's algorithm."
  (let* ((bits (bitset-bits bitset))
         (count 0)
         (vec (make-array (length bits) :element-type 'bit :displaced-to bits)))
    (dotimes (i (length vec) count)
      (when (= 1 (aref vec i))
        (incf count)))))

(defun bitset-parity (bitset)
  "Compute parity of bitset (0 if even number of set bits, 1 if odd)."
  (logand 1 (bitset-count bitset)))

(defun bitset-rank (bitset index)
  "Count number of set bits up to and including INDEX."
  (loop for i from 0 to index
        count (bitset-test bitset i)))

(defun bitset-select (bitset k)
  "Find position of the Kth set bit (1-indexed)."
  (let ((count 0))
    (do-bitset (i bitset nil)
      (incf count)
      (when (= count k) (return i)))))

;;; ============================================================================
;;; Bit Pattern Algorithms
;;; ============================================================================

(defun bitset-has-isolated-bits-p (bitset)
  "Check if bitset has any isolated set bits (no adjacent set bits)."
  (let* ((size (bitset-size bitset))
         (bits (bitset-bits bitset)))
    (loop for i from 0 below size
          when (and (= 1 (sbit bits i))
                   (> i 0) (= 1 (sbit bits (1- i))))
            return nil
          when (and (= 1 (sbit bits i))
                   (< i (1- size)) (= 1 (sbit bits (1+ i))))
            return nil
          finally (return t))))

(defun bitset-count-runs (bitset)
  "Count maximal runs of consecutive set bits."
  (let* ((size (bitset-size bitset))
         (bits (bitset-bits bitset))
         (run-count 0)
         (in-run nil))
    (dotimes (i size run-count)
      (let ((bit (= 1 (sbit bits i))))
        (when (and bit (not in-run))
          (incf run-count)
          (setf in-run t))
        (unless bit (setf in-run nil))))))

(defun bitset-run-lengths (bitset)
  "Return list of lengths of consecutive set bit runs."
  (let* ((size (bitset-size bitset))
         (bits (bitset-bits bitset))
         (runs nil)
         (current-run 0))
    (dotimes (i size (nreverse (if (> current-run 0)
                                   (cons current-run runs)
                                   runs)))
      (let ((bit (= 1 (sbit bits i))))
        (if bit
            (incf current-run)
            (when (> current-run 0)
              (push current-run runs)
              (setf current-run 0)))))))

(defun bitset-longest-run (bitset)
  "Find length of longest consecutive run of set bits."
  (let ((runs (bitset-run-lengths bitset)))
    (if runs (reduce #'max runs) 0)))

(defun bitset-is-power-of-two-p (bitset)
  "Check if bitset value is a power of two."
  (and (bitset-any-p bitset)
       (let* ((int-val (bitset-to-integer bitset))
              (minus-one (1- int-val)))
         (zerop (logand int-val minus-one)))))

(defun bitset-is-consecutive-p (bitset)
  "Check if all set bits are consecutive (single run)."
  (<= (bitset-count-runs bitset) 1))

(defun bitset-reverse-within-byte (bitset)
  "Reverse bit order within each byte of bitset."
  (let* ((bytes (bitset-to-bytes bitset))
         (result (make-array (length bytes) :element-type '(unsigned-byte 8))))
    (dotimes (i (length bytes) (bitset-from-bytes result))
      (let ((byte (aref bytes i))
            (reversed 0))
        (dotimes (j 8)
          (when (logbitp j byte)
            (setf reversed (logior reversed (ash 1 (- 7 j))))))
        (setf (aref result i) reversed)))))

;;; ============================================================================
;;; Bit Manipulation Patterns
;;; ============================================================================

(defun bitset-isolate-rightmost-bit (bitset)
  "Create bitset with only the rightmost set bit."
  (let ((int-val (bitset-to-integer bitset)))
    (bitset-from-integer (logand int-val (- int-val)))))

(defun bitset-clear-rightmost-bit (bitset)
  "Clear the rightmost set bit."
  (let ((int-val (bitset-to-integer bitset)))
    (bitset-from-integer (logand int-val (1- int-val)))))

(defun bitset-set-rightmost-clear-bit (bitset)
  "Set the rightmost clear bit."
  (let ((int-val (bitset-to-integer bitset)))
    (bitset-from-integer (logior int-val (1+ int-val)))))

(defun bitset-isolate-leftmost-bit (bitset)
  "Create bitset with only the leftmost set bit."
  (let ((msb (bitset-msb-position bitset)))
    (if (< msb 0)
        (bitset-from-integer 0)
        (bitset-from-integer (ash 1 msb)))))

(defun bitset-clear-leftmost-bit (bitset)
  "Clear the leftmost set bit."
  (let ((msb (bitset-msb-position bitset)))
    (if (< msb 0)
        bitset
        (let ((result (copy-bitset bitset)))
          (bitset-clear result msb)))))

(defun bitset-set-all-above (bitset index)
  "Set all bits above (not including) INDEX."
  (let ((result (copy-bitset bitset)))
    (loop for i from (1+ index) below (bitset-size result)
          do (bitset-set result i))
    result))

(defun bitset-set-all-below (bitset index)
  "Set all bits below (not including) INDEX."
  (let ((result (copy-bitset bitset)))
    (loop for i from 0 below index
          do (bitset-set result i))
    result))

(defun bitset-clear-all-above (bitset index)
  "Clear all bits above (not including) INDEX."
  (let ((result (copy-bitset bitset)))
    (loop for i from (1+ index) below (bitset-size result)
          do (bitset-clear result i))
    result))

(defun bitset-clear-all-below (bitset index)
  "Clear all bits below (not including) INDEX."
  (let ((result (copy-bitset bitset)))
    (loop for i from 0 below index
          do (bitset-clear result i))
    result))

;;; ============================================================================
;;; Statistical Operations
;;; ============================================================================

(defun bitset-statistics (bitset)
  "Return statistics about bitset as property list."
  (let* ((size (bitset-size bitset))
         (count (bitset-count bitset))
         (runs (bitset-count-runs bitset))
         (msb (bitset-msb-position bitset))
         (lsb (bitset-lsb-position bitset)))
    (list :size size
          :set-bits count
          :clear-bits (- size count)
          :density (float (/ count size))
          :runs runs
          :msb-position msb
          :lsb-position lsb
          :longest-run (bitset-longest-run bitset)
          :is-empty (bitset-empty-p bitset)
          :is-full (bitset-all-p bitset))))

(defun bitset-entropy (bitset)
  "Compute Shannon entropy of bitset."
  (let* ((size (bitset-size bitset))
         (ones (float (bitset-count bitset)))
         (zeros (float (- size ones)))
         (p1 (/ ones size))
         (p0 (/ zeros size)))
    (+ (if (zerop p1) 0 (* p1 (log p1 2)))
       (if (zerop p0) 0 (* p0 (log p0 2))))))

(defun bitset-gcd (bitset1 bitset2)
  "Compute GCD of two bitset values interpreted as integers."
  (let ((a (bitset-to-integer bitset1))
        (b (bitset-to-integer bitset2)))
    (loop while (not (zerop b))
          do (psetf a b b (mod a b)))
    (bitset-from-integer a)))

(defun bitset-lcm (bitset1 bitset2)
  "Compute LCM of two bitset values interpreted as integers."
  (let ((a (bitset-to-integer bitset1))
        (b (bitset-to-integer bitset2)))
    (bitset-from-integer (/ (* a b) (bitset-gcd bitset1 bitset2)))))

;;; ============================================================================
;;; Search and Matching
;;; ============================================================================

(defun bitset-substring-search (haystack needle)
  "Search for pattern NEEDLE in HAYSTACK bitset, return position or NIL."
  (let ((needle-size (bitset-size needle))
        (haystack-size (bitset-size haystack))
        (needle-int (bitset-to-integer needle)))
    (loop for i from 0 to (- haystack-size needle-size)
          for window-bits = (make-bitset needle-size)
          do (dotimes (j needle-size)
               (if (bitset-test haystack (+ i j))
                   (bitset-set window-bits j)
                   (bitset-clear window-bits j)))
          when (= (bitset-to-integer window-bits) needle-int)
            return i)))

(defun bitset-edit-distance (bitset1 bitset2)
  "Compute Hamming distance (edit distance for bits)."
  (bitset-hamming-distance bitset1 bitset2))

(defun bitset-match-wildcard (pattern text)
  "Match text against pattern with wildcard bits."
  ;; For this implementation, treat pattern as literal match
  (bitset-equal pattern text))

(defun bitset-all-permutations (bitset callback)
  "Generate all permutations of set bits in BITSET."
  (let* ((set-bits (bitset-to-list bitset))
         (n (length set-bits)))
    (labels ((permute (items)
               (if (<= (length items) 1)
                   (when items (funcall callback items))
                   (loop for i below (length items)
                         do (let ((first (nth i items))
                                  (rest (append (subseq items 0 i)
                                               (subseq items (1+ i)))))
                              (permute (cons first rest)))))))
      (permute set-bits))))

;;; ============================================================================
;;; I/O and Representation
;;; ============================================================================

(defun bitset-to-string (bitset &key (format :binary))
  "Convert bitset to string representation."
  (ecase format
    (:binary (bitset-to-binary bitset))
    (:hex (bitset-to-hex bitset))
    (:list (format nil "~A" (bitset-to-list bitset)))
    (:integer (format nil "~D" (bitset-to-integer bitset)))))

(defun bitset-from-string (string &key (format :binary))
  "Create bitset from string representation."
  (ecase format
    (:binary (bitset-from-binary string))
    (:hex (bitset-from-hex string))
    (:list (list-to-bitset (read-from-string string)))))

(defun bitset-print (bitset &optional (stream t) (format :binary))
  "Print bitset to stream in specified format."
  (format stream "~A" (bitset-to-string bitset :format format)))

(defun bitset-format-compact (bitset)
  "Return compact string representation using run-length encoding."
  (let* ((size (bitset-size bitset))
         (result nil)
         (current-bit nil)
         (run-length 0))
    (dotimes (i size)
      (let ((bit (bitset-test bitset i)))
        (if (eq bit current-bit)
            (incf run-length)
            (progn
              (when (not (null current-bit))
                (push (format nil "~A:~D" (if current-bit "1" "0") run-length) result))
              (setf current-bit bit)
              (setf run-length 1)))))
    (when (not (null current-bit))
      (push (format nil "~A:~D" (if current-bit "1" "0") run-length) result))
    (format nil "[~A]" (format nil "~{~A~^ ~}" (nreverse result)))))

;;; ============================================================================
;;; Advanced Bitset Analysis and Transformation
;;; ============================================================================

(defun bitset-gray-code (bitset)
  "Convert bitset integer value to Gray code representation.
   Gray code is useful for error detection in binary sequences."
  (let ((int-val (bitset-to-integer bitset)))
    (bitset-from-integer (logxor int-val (ash int-val -1)))))

(defun bitset-inverse-gray-code (gray-bitset)
  "Convert Gray code bitset back to binary representation."
  (let ((gray-int (bitset-to-integer gray-bitset))
        (result 0))
    (dotimes (i 64)
      (setf result (logxor result (ash gray-int (- i))))
      (when (zerop (logand gray-int (ash 1 i)))
        (return)))
    (bitset-from-integer result)))

(defun bitset-bit-reverse (bitset width)
  "Reverse bits within WIDTH-bit boundaries (e.g., reverse bits 0-7, 8-15, etc)."
  (let ((result (make-bitset (bitset-size bitset)))
        (size (bitset-size bitset)))
    (do ((block-start 0 (+ block-start width)))
        ((>= block-start size) result)
      (let ((block-end (min (+ block-start width) size)))
        (loop for i from block-start below block-end
              for rev-idx = (- (+ block-start width) 1 (- i block-start))
              when (and (< rev-idx size) (bitset-test bitset i))
                do (bitset-set result rev-idx))))))

(defun bitset-swap-nibbles (bitset)
  "Swap high and low nibbles (4-bit groups) within each byte.
   E.g., 0xAB becomes 0xBA."
  (let* ((bytes (bitset-to-bytes bitset))
         (result (make-array (length bytes) :element-type '(unsigned-byte 8))))
    (dotimes (i (length bytes) (bitset-from-bytes result))
      (let ((byte (aref bytes i))
            (low (logand (aref bytes i) #x0F))
            (high (ash (aref bytes i) -4)))
        (setf (aref result i) (logior (ash low 4) high))))))

(defun bitset-bit-interleave (bitset1 bitset2)
  "Interleave bits from two bitsets: b1[0]b2[0]b1[1]b2[1]...
   Assumes both bitsets have same size. Result size is doubled."
  (let* ((size (bitset-size bitset1))
         (result (make-bitset (* 2 size))))
    (dotimes (i size result)
      (when (bitset-test bitset1 i)
        (bitset-set result (* 2 i)))
      (when (bitset-test bitset2 i)
        (bitset-set result (1+ (* 2 i)))))))

(defun bitset-bit-deinterleave (bitset)
  "Deinterleave bits from a bitset into two bitsets.
   Even bits go to first, odd bits go to second."
  (let* ((size (bitset-size bitset))
         (out-size (ash size -1))
         (bitset1 (make-bitset out-size))
         (bitset2 (make-bitset out-size)))
    (dotimes (i out-size (values bitset1 bitset2))
      (when (bitset-test bitset (* 2 i))
        (bitset-set bitset1 i))
      (when (bitset-test bitset (1+ (* 2 i)))
        (bitset-set bitset2 i)))))

(defun bitset-count-bit-transitions (bitset)
  "Count number of transitions (0->1 or 1->0) in the bitset."
  (let ((size (bitset-size bitset))
        (transitions 0))
    (when (> size 1)
      (let ((prev-bit (bitset-test bitset 0)))
        (dotimes (i (1- size) transitions)
          (let ((curr-bit (bitset-test bitset (1+ i))))
            (when (not (eq prev-bit curr-bit))
              (incf transitions))
            (setf prev-bit curr-bit)))))))

(defun bitset-find-all-patterns (haystack pattern)
  "Find all positions where PATTERN appears in HAYSTACK.
   Returns list of starting indices."
  (let ((pattern-size (bitset-size pattern))
        (haystack-size (bitset-size haystack))
        (pattern-int (bitset-to-integer pattern))
        (matches nil))
    (loop for i from 0 to (- haystack-size pattern-size)
          for window-bits = (make-bitset pattern-size)
          do (dotimes (j pattern-size)
               (if (bitset-test haystack (+ i j))
                   (bitset-set window-bits j)
                   (bitset-clear window-bits j)))
          when (= (bitset-to-integer window-bits) pattern-int)
            do (push i matches))
    (nreverse matches)))

(defun bitset-contains-subsequence-p (bitset sequence)
  "Check if SEQUENCE (list of bit positions) appears consecutively in BITSET.
   SEQUENCE is a list like (1 2 3) meaning bits at positions 1,2,3 must be set
   and consecutive, with no intervening set bits."
  (if (null sequence)
      t
      (let ((first (first sequence))
            (rest (rest sequence)))
        (do-bitset (i bitset nil)
          (when (= i first)
            (let ((match-p t)
                  (curr-pos first))
              (dolist (seq-bit rest)
                (let ((expected-pos (+ (- seq-bit first) first)))
                  (unless (and (< expected-pos (bitset-size bitset))
                               (bitset-test bitset expected-pos))
                    (setf match-p nil))))
              (when match-p (return t))))))))

(defun bitset-autocorrelation (bitset)
  "Compute autocorrelation of the bitset.
   Returns number of set bits matching at each shift distance."
  (let ((size (bitset-size bitset))
        (result (make-array (1+ size) :element-type 'fixnum :initial-element 0)))
    (dotimes (shift (1+ size) result)
      (let ((count 0))
        (dotimes (i (- size shift))
          (when (and (bitset-test bitset i)
                     (bitset-test bitset (+ i shift)))
            (incf count)))
        (setf (aref result shift) count)))))

(defun bitset-periodic-p (bitset period)
  "Check if bitset has a periodic pattern with given PERIOD.
   Returns T if the pattern repeats exactly."
  (let ((size (bitset-size bitset)))
    (when (> period 0)
      (loop for i from 0 below (- size period)
            always (eq (bitset-test bitset i)
                       (bitset-test bitset (+ i period)))))))

(defun bitset-find-period (bitset)
  "Find the smallest period of the bitset pattern, or NIL if non-periodic.
   Tests periods from 1 to size/2."
  (let ((size (bitset-size bitset)))
    (dotimes (period (/ size 2))
      (when (> period 0)
        (when (bitset-periodic-p bitset period)
          (return period))))))

(defun bitset-majority-function (bitsets)
  "Return bitset where each bit is set if majority of input bitsets have that bit set.
   Requires all bitsets to have same size."
  (when (null bitsets)
    (error "Cannot compute majority of empty bitset list"))
  (let* ((size (bitset-size (first bitsets)))
         (result (make-bitset size))
         (threshold (ceiling (length bitsets) 2)))
    (dotimes (i size result)
      (let ((count 0))
        (dolist (bs bitsets)
          (when (bitset-test bs i)
            (incf count)))
        (when (>= count threshold)
          (bitset-set result i))))))

(defun bitset-xor-all (bitsets)
  "Compute XOR of all bitsets in list.
   Returns bitset where bit is set if odd number of inputs have it set."
  (when (null bitsets)
    (error "Cannot XOR empty bitset list"))
  (let ((result (copy-bitset (first bitsets))))
    (dolist (bs (rest bitsets) result)
      (setf (bitset-bits result)
            (bit-xor (bitset-bits result)
                     (bitset-bits bs)
                     (bitset-bits result))))))

(defun bitset-popcount-lookup (bitset)
  "Alternative population count using byte-lookup optimization.
   Precomputes counts for each byte value."
  (let* ((lookup (make-array 256 :element-type 'fixnum))
         (bytes (bitset-to-bytes bitset))
         (total 0))
    ;; Precompute population counts for 0-255
    (dotimes (i 256)
      (let ((count 0)
            (val i))
        (dotimes (j 8)
          (when (logbitp j val)
            (incf count)))
        (setf (aref lookup i) count)))
    ;; Sum up byte counts
    (dotimes (i (length bytes) total)
      (incf total (aref lookup (aref bytes i))))))

(defun bitset-hamming-weight-sum (bitsets)
  "Sum hamming weights (population counts) of multiple bitsets."
  (reduce #'+ bitsets :key #'bitset-count :initial-value 0))

(defun bitset-common-bits-count (bitset1 bitset2)
  "Count number of positions where both bitsets have set bits."
  (bitset-count (bitset-and bitset1 bitset2)))

(defun bitset-exclusive-bits-count (bitset1 bitset2)
  "Count bits that are set in exactly one of the two bitsets."
  (bitset-count (bitset-xor bitset1 bitset2)))

(defun bitset-union-cardinality (bitsets)
  "Count total number of distinct set bit positions across all bitsets."
  (bitset-count (bitset-union bitsets)))
