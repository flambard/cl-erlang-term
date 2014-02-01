(in-package :erlang-term)

;;;;
;;;; Erlang binary
;;;;

(defclass erlang-binary (erlang-object)
  ((bytes :reader bytes
          :initarg :bytes
          :documentation "Returns a vector of bytes from an Erlang binary.")
   (bits :reader bits-in-last-byte
         :initarg :bits
         :initform 8
         :documentation
         "The number of bits in the last byte of an Erlang binary."))
  (:documentation "Erlang binary."))


;;;
;;; Methods
;;;

(defmethod print-object ((object erlang-binary) stream)
  (print-unreadable-object (object stream :type t)
    (if (= 8 (bits-in-last-byte object))
        (format stream "<~{~s~^ ~}>" (coerce (bytes object) 'list))
        (format stream "<~{~s~^ ~}:~a>" (coerce (bytes object) 'list)
                (bits-in-last-byte object)))))

(defmethod initialize-instance :after ((object erlang-binary)
                                       &key (bits 8 bits-supplied-p) bytes
                                       &allow-other-keys)
  "Strip unused bits in last byte when making a bit-binary."
  (when bits-supplied-p
    (let ((length (length bytes)))
      (when (< 0 length)
        (symbol-macrolet ((last-byte (aref bytes (1- length))))
          (setf last-byte (keep-bits last-byte bits)))))))

(defun binary (&rest bytes)
  "Creates an Erlang binary from BYTES."
  (assert (every #'(lambda (b) (typep b '(unsigned-byte 8))) bytes))
  (make-instance 'erlang-binary :bytes (coerce bytes 'vector)))

(define-compiler-macro binary (&rest bytes)
  (if (every #'(lambda (byte) (typep byte '(unsigned-byte 8))) bytes)
      (make-instance 'erlang-binary :bytes (coerce bytes 'vector))
      `(make-instance 'erlang-binary :bytes (vector ,@bytes)) ))

(defun string-to-binary (string)
  "Creates an Erlang binary from the characters in STRING."
  (make-instance 'erlang-binary :bytes (string-to-byte-vector string)))

(define-compiler-macro string-to-binary (&whole form string)
  (if (stringp string)
      (make-instance 'erlang-binary :bytes (string-to-byte-vector string))
      form))

(defun bytes-to-binary (bytes)
  "Creates an Erlang binary from BYTES."
  (assert (every #'(lambda (b) (typep b '(unsigned-byte 8))) bytes))
  (make-instance 'erlang-binary :bytes (coerce bytes 'vector)))

(defun binary-to-string (binary)
  "Translates the bytes in BINARY to an ASCII string."
  (bytes-to-string (bytes binary)))

(defmethod size ((x erlang-binary))
  "The byte-size of Erlang binary X."
  (length (bytes x)))

(defmethod match-p ((a erlang-binary) (b erlang-binary))
  (let ((a-bytes (bytes a))
        (b-bytes (bytes b)))
    (and (alexandria:length= a-bytes b-bytes)
         (every #'= (bytes a) (bytes b)))))


;;;
;;; Encode/Decode
;;;

(defmethod encode ((x erlang-binary) &key &allow-other-keys)
  (if (= 8 (bits-in-last-byte x))
      (encode-external-binary x)
      (encode-external-bit-binary x)))


;; BINARY_EXT
;; +-----+-----+------+
;; |  1  |  4  |  Len |
;; +-----+-----+------+
;; | 109 | Len | Data |
;; +-----+-----+------+
;;

(defun encode-external-binary (erlang-binary)
  (with-slots (bytes) erlang-binary
    (concatenate 'nibbles:simple-octet-vector
                 (vector +binary-ext+)
                 (uint32-to-bytes (length bytes))
                 bytes)))

(defun decode-external-binary (bytes &optional (pos 0))
  (let ((length (bytes-to-uint32 bytes pos))
        (pos4 (+ 4 pos)))
    (values (make-instance 'erlang-binary
                           :bytes (subseq bytes pos4 (+ pos4 length)))
            (+ pos4 length))))


;; BIT_BINARY_EXT
;; +----+-----+------+------+
;; |  1 |  4  |   1  |  Len |
;; +----+-----+------+------+
;; | 77 | Len | Bits | Data |
;; +----+-----+------+------+
;;

(defun encode-external-bit-binary (erlang-binary)
  (with-slots (bytes bits) erlang-binary
    (concatenate 'nibbles:simple-octet-vector
                 (vector +bit-binary-ext+)
                 (uint32-to-bytes (length bytes))
                 (vector bits)
                 bytes)))

(defun decode-external-bit-binary (bytes &optional (pos 0))
  (let* ((length (bytes-to-uint32 bytes pos))
         (bits (aref bytes (+ 4 pos)))
         (pos5 (+ pos 5))
         (end-pos (+ pos5 length)))
    (values (make-instance 'erlang-binary
                           :bits bits
                           :bytes (subseq bytes pos5 end-pos))
            end-pos)))


;;; Helper functions

(defun keep-bits (byte bits)
  (let ((unused-bits (- 8 bits)))
    (ash (ldb (byte bits unused-bits) byte) unused-bits)))
