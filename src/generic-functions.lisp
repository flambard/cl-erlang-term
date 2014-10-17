(in-package :erlang-term)

;;;;
;;;; ENCODE - For encoding Erlang terms
;;;;

(defgeneric encode-erlang-object (erlang-translatable-object)
  (:documentation "Encodes the Erlang translatable object to a byte vector."))


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
