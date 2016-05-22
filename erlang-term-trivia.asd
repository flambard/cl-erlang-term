(asdf:defsystem :erlang-term-trivia
  :description "Trivia extensions for matching Erlang terms."
  :depends-on (:erlang-term :trivia)
  :components
  ((:module :trivia
            :components
            ((:file "package")
             (:file "binary"
                    :depends-on ("package"))
             (:file "erlang-string"
                    :depends-on ("package"))
             (:file "tuple"
                    :depends-on ("package"))
             ))))
