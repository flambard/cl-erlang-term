(defpackage #:erlang-term-test
  (:documentation "")
  (:use #:cl #:erlang-term #:etf-bops #:fiveam)
  (:shadow #:run-all-tests)
  (:export

   #:run-all-tests

   ))

(in-package :erlang-term-test)

(def-suite bops)
(def-suite decode)
(def-suite encode)
(def-suite erlang-object)
(def-suite erlang-translatable)

(defun run-all-tests ()
  (run! 'bops)
  (run! 'decode)
  (run! 'encode)
  (run! 'erlang-object)
  (run! 'erlang-translatable))
