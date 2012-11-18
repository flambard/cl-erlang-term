(in-package :erlang-term-test)

(in-suite bops)

(test unsigned-integer-to-bytes
  (is (equalp #() (unsigned-integer-to-bytes 0 0)))
  (is (equalp #(0) (unsigned-integer-to-bytes 0 1)))
  (is (equalp #(0 0 0 0) (unsigned-integer-to-bytes 0 4)))
  (is (equalp #(0 0 0 45) (unsigned-integer-to-bytes 45 4)))
  (is (equalp #(0 0 1 255) (unsigned-integer-to-bytes 511 4)))
  (is (equalp #(255) (unsigned-integer-to-bytes 511 1)))
  )

(test bytes-to-unsigned-integer
  (is (= 0 (bytes-to-unsigned-integer #(1 2 3) 0)))
  (is (= 0 (bytes-to-unsigned-integer #(0))))
  (is (= 1 (bytes-to-unsigned-integer #(1 2 3) 1)))
  (is (= 2 (bytes-to-unsigned-integer #(1 2 3) 1 1)))
  (is (= 515 (bytes-to-unsigned-integer #(1 2 3) 2 1)))
  )

(test string-to-bytes
  (is (equalp #() (string-to-bytes "")))
  (is (equalp #(65 66 66 65) (string-to-bytes "ABBA")))
  )

(test bytes-to-string
  (is (string= "" (bytes-to-string #())))
  (is (string= "ABBA" (bytes-to-string #(65 66 66 65))))
  )
