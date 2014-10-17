(in-package :erlang-term)

;;;;
;;;; Encode Erlang term
;;;;

(defun encode (x &key (version-tag +protocol-version+) compressed)
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
