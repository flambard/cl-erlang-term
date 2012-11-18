(asdf:defsystem :erlang-term-test
  :description "Unit-tests for cl-erlang-term."
  :depends-on (:erlang-term :fiveam)
  :components
  ((:module :test
            :components
            ((:file "package")
             (:file "bops-tests"
                    :depends-on ("package"))
             (:file "erlang-object-tests"
                    :depends-on ("package"))
             (:file "erlang-translatable-tests"
                    :depends-on ("package"))
             ))))
