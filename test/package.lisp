(defpackage #:erlang-term-test
  (:documentation "Unit tests for cl-erlang-term.")
  (:nicknames #:etf-test)
  (:use #:cl #:erlang-term #:etf-bops #:fiveam)
  (:shadow #:run-all-tests #:fail)
  (:export

   #:all-tests
   #:run-all-tests

   ))

(in-package :erlang-term-test)

(def-suite all-tests)

(def-suite bops                :in all-tests)
(def-suite decode              :in all-tests)
(def-suite encode              :in all-tests)
(def-suite erlang-object       :in all-tests)
(def-suite erlang-translatable :in all-tests)

(defun run-all-tests ()
  (run! 'all-tests))
