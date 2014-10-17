(in-package :erlang-term)

;;;;
;;;; Erlang float
;;;;

;;;
;;; Methods
;;;

(defmethod match-p ((a float) (b float))
  (= a b))


;;;
;;; Encode/Decode
;;;

(defmethod encode-erlang-object ((x float))
  ;; Should the old FLOAT_EXT ever be used?
  (encode-external-new-float (coerce x 'double-float)))


;; NEW_FLOAT_EXT
;; +----+------------+
;; |  1 |      8     |
;; +----+------------+
;; | 70 | IEEE float |
;; +----+------------+
;;

(defun encode-external-new-float (double-float)
  (concatenate 'nibbles:simple-octet-vector
               (vector +new-float-ext+)
               (double-float-to-bytes double-float)))

(defun decode-external-new-float (bytes &optional (pos 0))
  (values (bytes-to-double-float (subseq bytes pos))
          (+ 8 pos)))


;; FLOAT_EXT (superseded by NEW_FLOAT_EXT)
;; +----+--------------+
;; |  1 |      31      |
;; +----+--------------+
;; | 99 | Float String |
;; +----+--------------+
;;
;; The float is stored in string format, created with sprintf with format "%.20e".
;; To unpack the float use sscanf with format "%lf".

(defun encode-external-float (float)
  (concatenate 'nibbles:simple-octet-vector
               (vector +float-ext+)
               (string-to-byte-vector (format nil "~(~,20E~)" float))))

(defun decode-external-float (bytes &optional (pos 0))
  (let* ((padding-pos (position 0 bytes :start pos))
         (string (bytes-to-string bytes (- padding-pos pos) pos)))
    (values (read-from-string string)
            (+ 31 pos))))
