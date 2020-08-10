(asdf:defsystem :erlang-term-trivia
  :description "Trivia extensions for matching Erlang terms."
  :depends-on (:erlang-term :trivia)
  :components
  ((:module :trivia
            :components
            ((:file "package")
             (:file "impl" :depends-on ("package"))))))
