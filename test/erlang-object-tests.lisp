(in-package :erlang-term-test)

(in-suite erlang-object)

(test erlang-binary
  ;; Binary to string
  (is (string= "Test" (binary-to-string (string-to-binary "Test"))))
  (is (string= "" (binary-to-string (binary))))
  ;; Binary to byte vector
  (is (equalp #() (bytes (binary))))
  (is (equalp #(1 2 3) (bytes (binary 1 2 3))))
  (is (equalp #(1 2 3) (bytes (bytes-to-binary #(1 2 3)))))
  ;; Binary size
  (is (= 0 (size (binary))))
  (is (= 4 (size (string-to-binary "Test"))))
  ;; Matching binaries
  (is (match-p (string-to-binary "ABBA") (binary 65 66 66 65)))
  ;; Mismatching binaries
  (is (not (match-p (string-to-binary "ABBA") (binary 65 66 66 65 65))))
  (is (not (match-p (string-to-binary "ABBA") (binary 65 66 65 66))))
  ;; Translatable
  (is (erlang-translatable-p (binary)))
  (is (erlang-translatable-p (binary 0 0 0 0 1 8)))
  )

(test erlang-pid
  ;; Matching PIDs
  (is (match-p (make-instance 'erlang-pid
                              :node (make-symbol "localnode@localhost")
                              :id (uint32-to-bytes 42)
                              :serial (uint32-to-bytes 0)
                              :creation 1)
               (make-instance 'erlang-pid
                              :node (make-symbol "localnode@localhost")
                              :id (uint32-to-bytes 42)
                              :serial (uint32-to-bytes 0)
                              :creation 1)))
  ;; Mismatch on node name
  (is (not (match-p (make-instance 'erlang-pid
                                   :node (make-symbol "localnode@localhost")
                                   :id (uint32-to-bytes 42)
                                   :serial (uint32-to-bytes 0)
                                   :creation 1)
                    (make-instance 'erlang-pid
                                   :node (make-symbol "othernode@localhost")
                                   :id (uint32-to-bytes 42)
                                   :serial (uint32-to-bytes 0)
                                   :creation 1))))
  ;; Mismatch on id
  (is (not (match-p (make-instance 'erlang-pid
                                   :node (make-symbol "localnode@localhost")
                                   :id (uint32-to-bytes 42)
                                   :serial (uint32-to-bytes 0)
                                   :creation 1)
                    (make-instance 'erlang-pid
                                   :node (make-symbol "localnode@localhost")
                                   :id (uint32-to-bytes 946)
                                   :serial (uint32-to-bytes 0)
                                   :creation 1))))
  ;; Mismatch on serial
  (is (not (match-p (make-instance 'erlang-pid
                                   :node (make-symbol "localnode@localhost")
                                   :id (uint32-to-bytes 42)
                                   :serial (uint32-to-bytes 0)
                                   :creation 1)
                    (make-instance 'erlang-pid
                                   :node (make-symbol "localnode@localhost")
                                   :id (uint32-to-bytes 42)
                                   :serial (uint32-to-bytes 256)
                                   :creation 1))))
  ;; Mismatch on creation
  (is (not (match-p (make-instance 'erlang-pid
                                   :node (make-symbol "localnode@localhost")
                                   :id (uint32-to-bytes 42)
                                   :serial (uint32-to-bytes 0)
                                   :creation 1)
                    (make-instance 'erlang-pid
                                   :node (make-symbol "localnode@localhost")
                                   :id (uint32-to-bytes 42)
                                   :serial (uint32-to-bytes 0)
                                   :creation 2))))
  ;; Translatable
  (is (erlang-translatable-p
       (make-instance 'erlang-pid
                      :node (make-symbol "localnode@localhost")
                      :id (uint32-to-bytes 42)
                      :serial (uint32-to-bytes 0)
                      :creation 1)))
  )

(test erlang-reference
  ;; Matching refs
  (is (match-p (make-instance 'erlang-reference
                              :node (make-symbol "localnode@localhost")
                              :id (uint32-to-bytes 42)
                              :creation 1)
               (make-instance 'erlang-reference
                              :node (make-symbol "localnode@localhost")
                              :id (uint32-to-bytes 42)
                              :creation 1)))
  ;; Mismatch on node name
  (is (not (match-p (make-instance 'erlang-reference
                                   :node (make-symbol "othernode@localhost")
                                   :id (uint32-to-bytes 42)
                                   :creation 1)
                    (make-instance 'erlang-reference
                                   :node (make-symbol "localnode@localhost")
                                   :id (uint32-to-bytes 42)
                                   :creation 1))))
  ;; Mismatch on id
  (is (not (match-p (make-instance 'erlang-reference
                                   :node (make-symbol "localnode@localhost")
                                   :id (uint32-to-bytes 1024)
                                   :creation 1)
                    (make-instance 'erlang-reference
                                   :node (make-symbol "localnode@localhost")
                                   :id (uint32-to-bytes 42)
                                   :creation 1))))
  ;; Mismatch on creation
  (is (not (match-p (make-instance 'erlang-reference
                                   :node (make-symbol "localnode@localhost")
                                   :id (uint32-to-bytes 42)
                                   :creation 50)
                    (make-instance 'erlang-reference
                                   :node (make-symbol "localnode@localhost")
                                   :id (uint32-to-bytes 42)
                                   :creation 1))))
  ;; Translatable
  (is (erlang-translatable-p
       (make-instance 'erlang-reference
                      :node (make-symbol "localnode@localhost")
                      :id (uint32-to-bytes 42)
                      :creation 1)))
  )

(test erlang-tuple
  ;; Tuple arity
  (is (= 0 (arity (tuple))))
  (is (= 4 (arity (tuple 1 2 3 4))))
  (is (= 1 (arity (tuple (tuple)))))
  (is (= 2 (arity (tuple "hello" (list 7 7 7)))))
  ;; Tuple size
  (is (= 0 (size (tuple))))
  (is (= 4 (size (tuple 1 2 3 4))))
  (is (= 1 (size (tuple (tuple)))))
  (is (= 2 (size (tuple "hello" (list 7 7 7)))))
  (is (= 1 (size (tuple (binary 222 173 190 239)))))
  ;; Tuple elements vector
  (is (typep (elements (tuple t nil)) 'vector))
  (is (= 7 (length (elements (tuple 1 2 3 4 5 6 7)))))
  ;; Matching tuples
  (is (match-p (tuple) (tuple)))
  (is (match-p (tuple 42 "hello" (list 1 2 3))
               (tuple 42 "hello" (list 1 2 3))))
  ;; Mismatching tuples
  (is (not (match-p (tuple) (tuple 0))))
  (is (not (match-p (tuple 13) (tuple 31))))
  ;; Translatable
  (is (erlang-translatable-p (tuple)))
  (is (erlang-translatable-p (tuple (tuple 1 2) (tuple))))
  )
