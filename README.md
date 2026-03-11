# cl-bitset

Pure Common Lisp efficient bitset operations library with zero external dependencies.

## Installation

```lisp
(asdf:load-system :cl-bitset)
```

## Usage

```lisp
(use-package :cl-bitset)

;; Create a bitset
(let ((bs (make-bitset 100)))
  ;; Set bits
  (bitset-set bs 5)
  (bitset-set bs 10)
  (bitset-set bs 15)

  ;; Test bits
  (bitset-test bs 5)   ; => T
  (bitset-test bs 6)   ; => NIL

  ;; Count set bits
  (bitset-count bs)    ; => 3

  ;; Convert to list
  (bitset-to-list bs)) ; => (5 10 15)

;; Bitwise operations
(let ((a (list-to-bitset '(1 2 3)))
      (b (list-to-bitset '(2 3 4))))
  (bitset-to-list (bitset-and a b))  ; => (2 3)
  (bitset-to-list (bitset-or a b))   ; => (1 2 3 4)
  (bitset-to-list (bitset-xor a b))) ; => (1 4)

;; Iterate over set bits
(do-bitset (i bitset)
  (format t "Bit ~D is set~%" i))
```

## API

- `make-bitset` - Create new bitset
- `bitset-set`, `bitset-clear`, `bitset-test`, `bitset-toggle` - Bit manipulation
- `bitset-and`, `bitset-or`, `bitset-xor`, `bitset-not` - Bitwise operations
- `bitset-count` - Count set bits
- `bitset-to-list`, `list-to-bitset` - Conversion
- `do-bitset`, `map-bitset` - Iteration

## License

BSD-3-Clause. Copyright (c) 2024-2026 Parkian Company LLC.
