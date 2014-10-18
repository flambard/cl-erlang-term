(in-package :erlang-term)

(defconstant +compressed-term+ 80)


(defun zlib-compress (bytes)
  (concatenate 'nibbles:simple-octet-vector
               (vector +compressed-term+)
               (uint32-to-bytes (length bytes))
               (zlib:compress bytes :fixed)))


(defmethod decode-erlang-object ((tag (eql +compressed-term+)) bytes pos)
  (let* ((size (bytes-to-uint32 bytes pos))
         (uncompressed (zlib:uncompress (subseq bytes (+ 4 pos))
                                        :uncompressed-size size)))
    (decode-erlang-object (aref uncompressed 0) uncompressed 1)))
