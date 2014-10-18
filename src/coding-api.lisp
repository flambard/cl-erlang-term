(in-package :erlang-term)

;;;;
;;;; ENCODE
;;;;

(defun encode (x &key (version-tag +protocol-version+) compressed)
  "Encode an Erlang object into a sequence of bytes."
  (let ((bytes (encode-erlang-object x)))
    (when compressed
      (setf bytes (concatenate 'nibbles:simple-octet-vector
                               (vector +compressed-term+)
                               (uint32-to-bytes (length bytes))
                               (zlib:compress bytes :fixed))))
    (when (integerp version-tag)
      (setf bytes (concatenate 'nibbles:simple-octet-vector
                               (vector version-tag)
                               bytes)))
    bytes))


;;;;
;;;; DECODE
;;;;

(defun decode (bytes &key (start 0) (version-tag +protocol-version+))
  "Decode a sequence of bytes to an Erlang object."
  (when (integerp version-tag)
    (let ((version (aref bytes start)))
      (unless (= version version-tag)
        (error 'unexpected-message-tag-error
               :received-tag version
               :expected-tags (list version-tag))))
    (incf start))
  (let ((tag (aref bytes start)))
    (decode-erlang-object tag bytes (1+ start))))
