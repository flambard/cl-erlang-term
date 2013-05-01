(in-package :erlang-term-test)

(in-suite encode)

(test encode-atom
  ;; ATOM_CACHE_REF
  (let ((etf-aci:*atom-cache* (make-instance 'mock-atom-cache)))
    (is (equalp (nibbles:octet-vector 82 42) (encode :abba))) )
  ;; TODO: ATOM_EXT
  ;; TODO: SMALL_ATOM_EXT
  )

;; TODO: binary tests
;; TODO: float tests
;; TODO: fun tests
;; TODO: integer tests
;; TODO: list tests
;; TODO: pid tests
;; TODO: port tests
;; TODO: reference tests
;; TODO: string tests
;; TODO: tuple tests
