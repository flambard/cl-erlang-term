(in-package :erlang-term)

;;;;
;;;; Erlang atom
;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +atom-cache-ref+ 82)
  (defconstant +atom-ext+       100)
  (defconstant +small-atom-ext+ 115)
  )


;;;
;;; Methods
;;;

(defun make-atom (string)
  (if *atom-symbol-package*
      (intern string *atom-symbol-package*)
      (make-symbol string)))

(defmethod match-p ((a symbol) (b symbol))
  (string= (symbol-name a) (symbol-name b)))


;;;
;;; Encode/Decode
;;;

(defmethod encode-erlang-object ((x symbol))
  (cond
    ((and (null x) *lisp-nil-is-erlang-empty-list*)
     (encode-external-nil))
    ((and (null x) *lisp-nil-is-erlang-false*)
     (encode :|false| :version-tag nil))
    ((and (eq t x) *lisp-t-is-erlang-true*)
     (encode :|true| :version-tag nil))
    (t
     (let ((index (when etf-aci:*atom-cache*
                    (etf-aci:put-atom x etf-aci:*atom-cache*))))
       (cond
         (index ;; Use an atom cache reference
          (encode-external-atom-cache-ref index))
         ;; Encode the atom as usual
         ((> 256 (length (symbol-name x)))
          (encode-external-small-atom x))
         (t
          (encode-external-atom x)) ))) ))

(defmethod decode-erlang-object ((tag (eql +atom-cache-ref+)) bytes pos)
  (multiple-value-bind (sym new-pos) (decode-external-atom-cache-ref bytes pos)
    (values (translate-erlang-boolean sym) new-pos)))

(defmethod decode-erlang-object ((tag (eql +atom-ext+)) bytes pos)
  (decode-external-atom bytes pos))

(defmethod decode-erlang-object ((tag (eql +small-atom-ext+)) bytes pos)
  (multiple-value-bind (sym new-pos) (decode-external-small-atom bytes pos)
    (values (translate-erlang-boolean sym) new-pos)))

(defun decode-erlang-atom (bytes &optional (pos 0))
  (let ((tag (aref bytes pos)))
    (multiple-value-bind (symbol pos2)
        (case tag
          (#.+atom-cache-ref+
           (decode-external-atom-cache-ref bytes (1+ pos)))
          (#.+atom-ext+
           (decode-external-atom bytes (1+ pos)))
          (#.+small-atom-ext+
           (decode-external-small-atom bytes (1+ pos)))
          (otherwise
           (error 'unexpected-message-tag-error
                  :received-tag tag
                  :expected-tags (list +atom-cache-ref+
                                       +atom-ext+
                                       +small-atom-ext+))) )
      (values (translate-erlang-boolean symbol) pos2))))

(defun translate-erlang-boolean (symbol)
  (let ((name (symbol-name symbol)))
    (cond
      ((and (string= name "true") *erlang-true-is-lisp-t*) t)
      ((and (string= name "false") *erlang-false-is-lisp-nil*) nil)
      (t symbol))))


;; ATOM_CACHE_REF
;; +----+-------------------+
;; |  1 |         1         |
;; +----+-------------------+
;; | 82 | AtomCacheRefIndex |
;; +----+-------------------+
;;

(defun encode-external-atom-cache-ref (reference-index)
  (concatenate 'nibbles:simple-octet-vector
               (vector +atom-cache-ref+)
               (vector reference-index)))

(defun decode-external-atom-cache-ref (bytes &optional (pos 0))
  (unless etf-aci:*atom-cache* (error 'atom-cache-missing-error :bytes bytes))
  (multiple-value-bind (cached-atom present)
      (etf-aci:get-atom (aref bytes pos) etf-aci:*atom-cache*)
    (unless present
      (error 'atom-not-in-cache-error
             :bytes bytes
             :atom-reference cached-atom))
    (values cached-atom
            (1+ pos))))



;; ATOM_EXT
;; +-----+-----+----------+
;; |  1  |  2  |    Len   |
;; +-----+-----+----------+
;; | 100 | Len | AtomName |
;; +-----+-----+----------+
;;

(defun encode-external-atom (atom)
  (concatenate 'nibbles:simple-octet-vector
               (vector +atom-ext+)
               (uint16-to-bytes (length (symbol-name atom)))
               (string-to-byte-vector (symbol-name atom))))

(defun decode-external-atom (bytes &optional (pos 0))
  (let ((length (bytes-to-uint16 bytes pos))
        (pos2 (+ 2 pos)))
    (values (make-atom (bytes-to-string bytes length pos2))
            (+ pos2 length))))



;; SMALL_ATOM_EXT
;; +-----+-----+----------+
;; |  1  |  1  |    Len   |
;; +-----+-----+----------+
;; | 115 | Len | AtomName |
;; +-----+-----+----------+
;;

(defun encode-external-small-atom (atom)
  (concatenate 'nibbles:simple-octet-vector
               (vector +small-atom-ext+)
               (vector (length (symbol-name atom)))
               (string-to-byte-vector (symbol-name atom))))

(defun decode-external-small-atom (bytes &optional (pos 0))
  (let ((length (aref bytes pos))
        (pos1 (1+ pos)))
    (values (make-atom (bytes-to-string bytes length pos1))
            (+ pos1 length))))
