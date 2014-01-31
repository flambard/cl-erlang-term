(asdf:defsystem :erlang-term-test
  :description "Unit-tests for cl-erlang-term."
  :depends-on (:erlang-term :erlang-term-optima :fiveam :nibbles)
  :components
  ((:module :test
            :components
            ((:file "package")
             (:file "mock-atom-cache"
                    :depends-on ("package"))
             (:file "bops-tests"
                    :depends-on ("package"))
             (:file "decode-tests"
                    :depends-on ("package"
                                 "mock-atom-cache"))
             (:file "encode-tests"
                    :depends-on ("package"
                                 "mock-atom-cache"))
             (:file "erlang-object-tests"
                    :depends-on ("package"))
             (:file "erlang-translatable-tests"
                    :depends-on ("package"))
             (:file "optima-tests"
                    :depends-on ("package"))
             ))))
