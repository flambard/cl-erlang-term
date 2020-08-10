(asdf:defsystem :erlang-term-optima
  :description "(Legacy) Transitional system for optima extensions for matching Erlang terms"
  :depends-on (:erlang-term-trivia :alexandria)
  :components
  ((:module :optima
            :components
            ((:file "package")))))
