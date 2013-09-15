(in-package :erlang-term)

;;;;
;;;; ENCODE - For encoding Erlang terms
;;;;

(defgeneric encode (erlang-translatable-object &key version-tag compressed)
  (:documentation "Encodes the Erlang translatable object to a byte vector."))

(defmethod encode :around (x &key version-tag compressed)
  (let ((bytes (call-next-method x)))
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
;;;; MATCH-P - Predicate for comparing Erlang objects
;;;;

(defgeneric match-p (object-a object-b)
  (:documentation "Predicate for testing if two Erlang objects match."))

(defmethod match-p (a b)
  nil)


;;;;
;;;; ARITY
;;;;

(defgeneric arity (tuple-or-fun)
  (:documentation "Returns the arity of an Erlang tuple or fun."))


;;;;
;;;; SIZE
;;;;

(defgeneric size (tuple-or-binary)
  (:documentation "Returns the size of an Erlang tuple or binary."))
