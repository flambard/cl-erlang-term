(defpackage #:erlang-term-test
  (:documentation "")
  (:use #:cl #:fiveam #:erlang-term #:etf-bops)
  (:export
   ))

(in-package :erlang-term-test)

(def-suite bops)
(def-suite decode)
(def-suite encode)
(def-suite erlang-object)
(def-suite erlang-translatable)
