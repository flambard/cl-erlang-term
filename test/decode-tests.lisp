(in-package :erlang-term-test)

(in-suite decode)

(test decode-atom
  ;; TODO: ATOM_CACHE_REF
  ;; ATOM_EXT
  (is (symbolp (decode (nibbles:octet-vector 100 0 4 65 66 66 65))))
  (is (string= "ABBA" (symbol-name
                       (decode (nibbles:octet-vector 100 0 4 65 66 66 65)))))
  ;; SMALL_ATOM_EXT
  (is (symbolp (decode (nibbles:octet-vector 115 4 65 66 66 65))))
  (is (string= "ABBA" (symbol-name
                       (decode (nibbles:octet-vector 115 4 65 66 66 65)))))
  )

(test decode-binary
  ;; BINARY_EXT
  (is (typep (decode (nibbles:octet-vector 109 0 0 0 4 1 2 3 4))
             'erlang-binary))
  (is (= 0 (size (decode (nibbles:octet-vector 109 0 0 0 0)))))
  (is (string= "ABBA"
               (binary-to-string
                (decode (nibbles:octet-vector 109 0 0 0 4 65 66 66 65)))))
  ;; BIT_BINARY_EXT
  (is (typep (decode (nibbles:octet-vector 77 0 0 0 4 1 1 2 3 3))
             'erlang-binary))
  (is (= 0 (size (decode (nibbles:octet-vector 77 0 0 0 0 7)))))
  (is (equalp #(0) (bytes (decode (nibbles:octet-vector 77 0 0 0 1 7 1)))))
  (is (equalp #(1) (bytes (decode (nibbles:octet-vector 77 0 0 0 1 8 1)))))
  (is (equalp #(0) (bytes (decode (nibbles:octet-vector 77 0 0 0 1 0 255)))))
  (is (equalp #(128) (bytes (decode (nibbles:octet-vector 77 0 0 0 1 1 255)))))
  )

(test decode-float
  ;; NEW_FLOAT_EXT
  (is (typep (decode (nibbles:octet-vector 70 0 0 0 0 0 0 0 0)) 'float))
  (is (= 0.0 (decode (nibbles:octet-vector 70 0 0 0 0 0 0 0 0))))
  ;; FLOAT_EXT
  (is (typep (decode (concatenate
                      'nibbles:simple-octet-vector
                      #(99)
                      (string-to-bytes (format nil "~(~,20E~)" 4.56))
                      #(0 0 0 0 0)))
             'float))
  (is (= 4.56 (decode (concatenate
                       'nibbles:simple-octet-vector
                       #(99)
                       (string-to-bytes (format nil "~(~,20E~)" 4.56))
                       #(0 0 0 0 0)))))
  )

(test decode-fun
  ;; TODO: FUN_EXT
  ;; TODO: NEW_FUN_EXT
  ;; TODO: EXPORT_EXT
  )

(test decode-integer
  ;; SMALL_INTEGER_EXT
  (is (typep (decode (nibbles:octet-vector 97 97)) 'integer))
  (is (= 0 (decode (nibbles:octet-vector 97 0))))
  (is (= 255 (decode (nibbles:octet-vector 97 255))))
  ;; INTEGER_EXT
  (is (typep (decode (nibbles:octet-vector 98 97 44 52 3)) 'integer))
  (is (= 0 (decode (nibbles:octet-vector 98 0 0 0 0))))
  (is (= 2147483647 (decode (nibbles:octet-vector 98 127 255 255 255))))
  (is (= -1 (decode (nibbles:octet-vector 98 255 255 255 255))))
  ;; SMALL_BIG_EXT
  (is (typep (decode (nibbles:octet-vector 110 5 0 52 3 0 128 212)) 'integer))
  (is (= 0 (decode (nibbles:octet-vector 110 7 0 0 0 0 0 0 0 0))))
  (is (= -255 (decode (nibbles:octet-vector 110 1 1 255))))
  (is (= 4294967295 (decode (nibbles:octet-vector 110 4 0 255 255 255 255))))
  (is (= -2147483647 (decode (nibbles:octet-vector 110 4 1 255 255 255 127))))
  ;; LARGE_BIG_EXT
  (is (typep (decode (nibbles:octet-vector 111 0 0 0 5 0 52 3 0 128 212))
             'integer))
  (is (= 0 (decode (nibbles:octet-vector 111 0 0 0 7 0 0 0 0 0 0 0 0))))
  (is (= -255 (decode (nibbles:octet-vector 111 0 0 0 1 1 255))))
  (is (= 4294967295
         (decode (nibbles:octet-vector 111 0 0 0 4 0 255 255 255 255))))
  (is (= -2147483647
         (decode (nibbles:octet-vector 111 0 0 0 4 1 255 255 255 127))))
  )
