(in-package :erlang-term)

;;;;
;;;; Erlang string
;;;;

;;;
;;; Methods
;;;

(defmethod match-p ((a string) (b string))
  (string= a b))


;;;
;;; Encode/Decode
;;;

(defmethod encode-erlang-object ((x string))
  (cond (*lisp-string-is-erlang-binary*
         (encode (string-to-binary x) :version-tag nil))
        ((> 65536 (length x))
         (encode-external-string x))
        (t
         (encode-external-list (string-to-byte-list x)))))

(defmethod decode-erlang-object ((tag (eql +string-ext+)) bytes pos)
  (decode-external-string bytes pos))


;; STRING_EXT
;; +-----+--------+------------+
;; |  1  |    2   |   Length   |
;; +-----+--------+------------+
;; | 107 | Length | Characters |
;; +-----+--------+------------+
;;

(defun encode-external-string (chars)
  (concatenate 'nibbles:simple-octet-vector
               (vector +string-ext+)
               (uint16-to-bytes (length chars))
               (if (stringp chars)
                   (string-to-byte-vector chars)
                   (coerce chars 'vector))))

(defun decode-external-string (bytes &optional (pos 0))
  (let* ((length (bytes-to-uint16 bytes pos))
         (bytes (subseq bytes (+ 2 pos) (+ 2 length pos))))
    (values (if *erlang-string-is-lisp-string*
                (bytes-to-string bytes)
                (coerce bytes 'list))
            (+ 2 length pos))))
