(defpackage #:erlang-term-optima
  (:documentation "(Legacy) Transitional system for optima extensions for matching Erlang terms")
  (:nicknames #:etf-optima)
  (:use #:erlang-term-trivia)
  (:export

   #:erlang-string

   ))

(alexandria:simple-style-warning
 "Package ERLANG-TERM-OPTIMA is deprecated, due to the retirement of optima (removed from quicklisp).
Consider switching to ERLANG-TERM-TRIVIA")

;; 
