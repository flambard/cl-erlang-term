(defpackage #:etf-atom-cache-interface
  (:documentation "A generic atom cache interface. This interface is expected to
be implemented by an application that wants to use cached atom references.")
  (:nicknames #:etf-aci)
  (:use #:cl)
  (:export

   #:*atom-cache*
   #:get-atom
   #:put-atom

   ))

(defpackage #:etf-bops
  (:documentation "Byte operations.")
  (:use #:cl)
  (:export

   #:bytes-to-double-float
   #:bytes-to-signed-int32
   #:bytes-to-uint16
   #:bytes-to-uint32
   #:bytes-to-unsigned-integer

   #:read-bytes
   #:read-signed-int32
   #:read-string
   #:read-uint16
   #:read-uint32

   #:double-float-to-bytes
   #:signed-int32-to-bytes
   #:uint16-to-bytes
   #:uint32-to-bytes
   #:unsigned-integer-to-bytes

   #:write-signed-int32
   #:write-uint16
   #:write-uint32

   ))

(defpackage #:erlang-term
  (:documentation "Erlang External Term Format")
  (:nicknames #:etf)
  (:use #:cl #:etf-bops)
  (:export

   ;; Type
   #:erlang-translatable
   #:erlang-translatable-p

   ;; Classes
   #:erlang-binary
   #:erlang-external-fun
   #:erlang-old-internal-fun
   #:erlang-new-internal-fun
   #:erlang-pid
   #:erlang-port
   #:erlang-reference
   #:erlang-tuple

   ;; Class methods and functions
   #:encode
   #:decode
   #:make-atom
   #:match-p
   #:bytes
   #:bits-in-last-byte
   #:module
   #:arity
   #:size
   #:elements
   #:tuple
   #:tuple-arity
   #:tuple-ref
   #:binary
   #:string-to-binary
   #:bytes-to-binary
   #:binary-to-string
   #:node

   ;; Conditions
   #:not-implemented-error
   #:malformed-external-erlang-term-error
   #:untranslatable-lisp-object-error
   #:unexpected-message-tag-error

   ;; Special variables
   #:*atom-symbol-package*
   #:*lisp-t-is-erlang-true*
   #:*lisp-nil-is-erlang-empty-list*
   #:*lisp-nil-is-erlang-false*
   #:*lisp-nil-at-tail-is-erlang-empty-list*
   #:*lisp-string-is-erlang-binary*
   #:*erlang-true-is-lisp-t*
   #:*erlang-false-is-lisp-nil*
   #:*erlang-string-is-lisp-string*

   ))
