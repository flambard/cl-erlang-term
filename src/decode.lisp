(in-package :erlang-term)

;;;;
;;;; Decode Erlang term
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
