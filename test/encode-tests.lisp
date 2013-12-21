(in-package :erlang-term-test)

(in-suite encode)

(test encode-atom
  ;; ATOM_CACHE_REF
  (let ((etf-aci:*atom-cache* (make-instance 'mock-atom-cache)))
    (is (equalp (nibbles:octet-vector 131 82 42) (encode :abba))) )
  ;; TODO: ATOM_EXT
  ;; TODO: SMALL_ATOM_EXT
  )

(test encode-binary
  ;; BINARY_EXT
  (is (equalp (nibbles:octet-vector 131 109 0 0 0 4 1 2 3 4)
              (encode (binary 1 2 3 4))))
  (is (equalp (nibbles:octet-vector 131 109 0 0 0 0)
              (encode (binary))))
  (is (equalp (nibbles:octet-vector 131 109 0 0 0 4 65 66 66 65)
              (encode (string-to-binary "ABBA"))))
  ;; TODO: BIT_BINARY_EXT
  )

(test encode-float
  ;; NEW_FLOAT_EXT
  (is (equalp (nibbles:octet-vector 131 70 0 0 0 0 0 0 0 0)
              (encode 0.0)))
  (is (equalp (nibbles:octet-vector 131 70 64 9 30 184 96 0 0 0)
              (encode 3.14)))
  ;; FLOAT_EXT
  ;; Currently floats are never encoded to the FLOAT_EXT format.
  )

;; TODO: fun tests

(test encode-integer
  ;; SMALL_INTEGER_EXT
  (is (equalp (nibbles:octet-vector 131 97 0)
              (encode 0)))
  (is (equalp (nibbles:octet-vector 131 97 255)
              (encode 255)))

  ;; INTEGER_EXT
  (is (equalp (nibbles:octet-vector 131 98 0 0 1 0)
              (encode 256)))
  (is (equalp (nibbles:octet-vector 131 98 127 255 255 255)
              (encode (1- (expt 2 31)))))
  (is (equalp (nibbles:octet-vector 131 98 255 255 255 255)
              (encode -1)))
  (is (equalp (nibbles:octet-vector 131 98 128 0 0 0)
              (encode (- (expt 2 31)))))

  ;; SMALL_BIG_EXT
  (is (equalp (nibbles:octet-vector 131 110 4 0 0 0 0 128)
              (encode (expt 2 31))))
  (is (equalp (nibbles:octet-vector 131 110 4 0 255 255 255 255)
              (encode (1- (expt 2 32)))))
  (is (equalp (nibbles:octet-vector 131 110 4 1 1 0 0 128)
              (encode (- (1+ (expt 2 31))))))
  (is (equalp (nibbles:octet-vector 131 110 4 1 255 255 255 255)
              (encode (- (1- (expt 2 32))))))

  ;; LARGE_BIG_EXT
  (is (= 263 (length (encode (expt 2 2040)))))
  (is (= 263 (length (encode (- (expt 2 2040))))))
  (is (equalp (nibbles:octet-vector 131 111 0 0 1 0 0)
              (subseq (encode (expt 2 2040)) 0 7)))
  (is (equalp (nibbles:octet-vector 131 111 0 0 1 0 1)
              (subseq (encode (- (expt 2 2040))) 0 7)))
  )

(test encode-list
  ;; NIL_EXT
  (is (equalp (nibbles:octet-vector 131 106)
              (encode nil)))
  ;; LIST_EXT
  (is (equalp (nibbles:octet-vector 131 108 0 0 0 1 106 106)
              (encode (list nil))))
  (is (equalp (nibbles:octet-vector 131 108 0 0 0 3 97 1 97 2 97 3 106)
              (encode (list 1 2 3))))
  (is (equalp (nibbles:octet-vector 131 108 0 0 0 1 97 1 97 2)
              (encode (cons 1 2))))
  )

;; TODO: pid tests
;; TODO: port tests
;; TODO: reference tests

(test encode-string
  ;; STRING_EXT
  (let ((*lisp-string-is-erlang-binary* nil))
    (is (equalp (nibbles:octet-vector 131 107 0 0)
                (encode ""))))
  (let ((*lisp-string-is-erlang-binary* nil))
    (is (equalp (nibbles:octet-vector 131 107 0 4 65 66 66 65)
                (encode "ABBA"))))

  (let ((*lisp-string-is-erlang-binary* t))
    (is (equalp (nibbles:octet-vector 131 109 0 0 0 0)
                (encode ""))))
  (let ((*lisp-string-is-erlang-binary* t))
    (is (equalp (nibbles:octet-vector 131 109 0 0 0 4 65 66 66 65)
                (encode "ABBA"))))

  ;; Strings are encoded as strings (not binaries) by default
  (is (equalp (nibbles:octet-vector 131 107 0 0)
              (encode "")))
  (is (equalp (nibbles:octet-vector 131 107 0 4 65 66 66 65)
              (encode "ABBA")))
  )

(test encode-tuple
  ;; SMALL_TUPLE_EXT
  (is (equalp (nibbles:octet-vector 131 104 0)
      (encode (tuple))))
  (is (equalp (nibbles:octet-vector 131 104 1 104 0)
      (encode (tuple (tuple)))))
  (is (equalp (nibbles:octet-vector 131 104 2 97 1 97 2)
      (encode (tuple 1 2))))

  ;; TODO: LARGE_TUPLE_EXT
  )

(test encode-compressed
  (is (equalp (encode (tuple 1 2 3) :compressed t)
              (nibbles:octet-vector 131 80 0 0 0 8
                                    120 156 203 96 78 100 76
                                    100 74 100 6 0 7 247 1 149)))
  )
