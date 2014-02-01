(asdf:defsystem :erlang-term-optima
  :description "Optima extensions for matching Erlang terms."
  :depends-on (:erlang-term :optima)
  :components
  ((:module :optima
            :components
            ((:file "package")
             (:file "binary"
                    :depends-on ("package"))
             (:file "erlang-string"
                    :depends-on ("package"))
             (:file "tuple"
                    :depends-on ("package"))
             ))))
