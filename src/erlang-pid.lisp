(in-package :erlang-term)

;;;;
;;;; Erlang pid
;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +pid-ext+ 103)
  (defconstant +new-pid-ext+ 88)
  )


(defclass erlang-pid (erlang-identifier)
  ((serial :initarg :serial))
  (:documentation "Erlang PID."))


;;;
;;; Methods
;;;

(defun make-pid (node id serial creation)
  (make-instance 'erlang-pid
                 :node (make-symbol node)
                 :id id
                 :serial serial
                 :creation creation))

(defmethod print-object ((object erlang-pid) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (node id serial) object
      (format stream "~a <~a.~a>"
              node (bytes-to-uint32 id) (bytes-to-uint32 serial)))))

;; Pids in Erlang are printed like this <X.id.serial>
;; where X = some number representing the node.

;; Not mentioned in the documentation: Serial only uses the least significant 13 bits!

(defmethod match-p ((a erlang-pid) (b erlang-pid))
  (and (call-next-method)
       (every #'= (slot-value a 'serial) (slot-value b 'serial))))


;;;
;;; Encode/Decode
;;;

(defmethod encode-erlang-object ((x erlang-pid))
  (encode-external-pid x))

(defmethod decode-erlang-object ((tag (eql +pid-ext+)) bytes pos)
  (decode-external-pid bytes pos))

(defmethod decode-erlang-object ((tag (eql +new-pid-ext+)) bytes pos)
  (decode-external-pid bytes pos t))

(defun decode-erlang-pid (bytes &optional (pos 0))
  (let ((tag (aref bytes pos)))
    (case tag
      ((#.+pid-ext+ #.+new-pid-ext+)
       (decode-external-pid bytes (1+ pos) (eql tag #.+new-pid-ext+)))
      (otherwise
       (error 'unexpected-erlang-term
              :received-tag tag
              :expected-tags (list +pid-ext+ +new-pid-ext+))) )))


;; PID_EXT
;; +-----+------+----+--------+----------+
;; |  1  |   N  |  4 |    4   |     1    |
;; +-----+------+----+--------+----------+
;; | 103 | Node | ID | Serial | Creation |
;; +-----+------+----+--------+----------+
;;
;; NEW_PID_EXT
;; +-----+------+----+--------+----------+
;; |  1  |   N  |  4 |    4   |     4    |
;; +-----+------+----+--------+----------+
;; |  88 | Node | ID | Serial | Creation |
;; +-----+------+----+--------+----------+
;;

(defun encode-external-pid (pid)
  (with-slots (node id serial creation) pid
    (concatenate 'nibbles:simple-octet-vector
                 (vector +new-pid-ext+)
                 (encode-erlang-object node)
                 id
                 serial
                 (uint32-to-bytes creation))))

(defun decode-external-pid (bytes &optional (pos 0) (new-pid-ext nil))
  (multiple-value-bind (node pos1) (decode-erlang-atom bytes pos)
    (values (make-instance 'erlang-pid
                           :node node
                           :id (subseq bytes pos1 (+ pos1 4))
                           :serial (subseq bytes (+ pos1 4) (+ pos1 8))
                           :creation (if new-pid-ext
                                         (subseq bytes (+ pos1 8) (+ pos1 12))
                                         (aref bytes (+ pos1 8))))
            (if new-pid-ext
                (+ pos1 12)
                (+ pos1 9)))))
