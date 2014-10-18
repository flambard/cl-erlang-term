(in-package :erlang-term)

;;;;
;;;; ENCODE-ERLANG-OBJECT - For encoding Erlang terms
;;;;

(defgeneric encode-erlang-object (erlang-translatable-object)
  (:documentation "Encodes the Erlang translatable object to a byte vector."))


;;;;
;;;; DECODE-ERLANG-OBJECT - For decoding Erlang terms
;;;;

(defgeneric decode-erlang-object (tag bytes position)
  (:documentation "Decodes a byte vector into an Erlang translatable object."))


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
