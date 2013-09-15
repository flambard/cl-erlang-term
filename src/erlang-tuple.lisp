(in-package :erlang-term)

;;;;
;;;; Erlang tuple
;;;;

(defclass erlang-tuple (erlang-object)
  ((elements :reader elements :initarg :elements))
  (:documentation "Erlang tuple."))


;;;
;;; Methods
;;;

(defmethod print-object ((object erlang-tuple) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "{~{~s~^ ~}}" (coerce (elements object) 'list))))

(defun tuple (&rest erlang-translatable-objects)
  "Create an Erlang tuple"
  (make-instance 'erlang-tuple
                 :elements (coerce erlang-translatable-objects 'vector)))

(defun tuple-ref (tuple pos)
  (svref (elements tuple) pos))

(defun tuple-arity (tuple)
  (length (elements tuple)))

(defmethod arity ((x erlang-tuple))
  "The number of elements of Erlang tuple X."
  (tuple-arity x))

(defmethod size ((x erlang-tuple))
  "The number of elements of Erlang tuple X."
  (tuple-arity x))

(defmethod match-p ((a erlang-tuple) (b erlang-tuple))
  (and (= (arity a) (arity b))
       (every #'match-p (elements a) (elements b))))


;;;
;;; Encode/Decode
;;;

(defmethod encode ((x erlang-tuple) &key &allow-other-keys)
  (if (> 256 (tuple-arity x))
      (encode-external-small-tuple x)
      (encode-external-large-tuple x)))


;; SMALL_TUPLE_EXT
;; +-----+-------+----------+
;; |  1  |   1   |     N    |
;; +-----+-------+----------+
;; | 104 | Arity | Elements |
;; +-----+-------+----------+
;;

(defun encode-external-small-tuple (tuple)
  (concatenate 'nibbles:simple-octet-vector
               (vector +small-tuple-ext+ (tuple-arity tuple))
               (mapconc-vector #'encode (elements tuple))))

(defun decode-external-small-tuple (bytes &optional (pos 0))
  (let ((arity (aref bytes pos)))
    (multiple-value-bind (elements new-pos)
        (decode-tuple-contents bytes arity (1+ pos))
      (values (make-instance 'erlang-tuple :elements elements)
              new-pos))))


;; LARGE_TUPLE_EXT
;; +-----+-------+----------+
;; |  1  |   4   |     N    |
;; +-----+-------+----------+
;; | 105 | Arity | Elements |
;; +-----+-------+----------+
;;

(defun encode-external-large-tuple (tuple)
  (concatenate 'nibbles:simple-octet-vector
               (vector +large-tuple-ext+)
               (uint32-to-bytes (tuple-arity tuple))
               (mapconc-vector #'encode (elements tuple))))

(defun decode-external-large-tuple (bytes &optional (pos 0))
  (let ((arity (bytes-to-uint32 bytes pos)))
    (multiple-value-bind (elements new-pos)
        (decode-tuple-contents bytes arity (+ 4 pos))
      (values (make-instance 'erlang-tuple :elements elements)
              new-pos))))


;;; Helper functions

(defun mapconc-vector (fn vector)
  (loop
     with bytes = #()
     for element across vector
     do (setf bytes (concatenate 'nibbles:simple-octet-vector
                                 bytes
                                 (funcall fn element)))
     finally (return bytes)))

(defun decode-tuple-contents (bytes arity pos)
  (loop
     repeat arity
     for (element pos1) = (multiple-value-list
                           (decode bytes :start pos :version-tag nil))
     do (setf pos pos1)
     collect element into elements
     finally (return (values (coerce elements 'vector) pos))))
