(in-package :erlang-term-test)

(in-suite erlang-translatable)

(test erlang-atom
  ;; Atoms are symbols
  (is (symbolp (make-atom "")))
  (is (symbolp (make-atom "sym1")))
  ;; Atom symbol package
  (is (string= (symbol-name *atom-symbol-package*)
               (package-name (symbol-package (make-atom "sym2")))))
  ;; Default atom symbol package is KEYWORD
  (is (keywordp (make-atom "sym3")))
  ;; Setting a new atom symbol package
  (let ((*atom-symbol-package* :erlang-term-test))
    (is (string= (symbol-name :erlang-term-test)
                 (package-name (symbol-package (make-atom "sym4"))))))
  ;; Matching atoms
  (is (match-p (make-atom "sym5") (make-atom "sym5")))
  ;; Mismatching atoms
  (is (not (match-p (make-atom "sym6") (make-atom "sym66"))))
  (is (not (match-p (make-atom "sym7") (make-atom "sym8"))))
  ;; Translatable
  (is (erlang-translatable-p (make-atom "")))
  (is (erlang-translatable-p (make-atom "sym9")))
  )

(test erlang-float
  ;; Matching floats
  (is (match-p 367.89 367.89))
  ;; Mismatching floats
  (is (not (match-p 367.89 367.90)))
  ;; Translatable
  (is (erlang-translatable-p 0.0))
  (is (erlang-translatable-p 7.8))
  )

(test erlang-integer
  ;; Matching integers
  (is (match-p 42 42))
  ;; Mismatching integers
  (is (not (match-p 0 1)))
  ;; Translatable
  (is (erlang-translatable-p 0))
  (is (erlang-translatable-p 2480))
  )

(test erlang-list
  ;; Matching lists
  (is (match-p (list) (list)))
  (is (match-p (list (list) (list)) (list (list) (list))))
  (is (match-p (list 1 2 3) (list 1 2 3)))
  ;; Mismatching lists
  (is (not (match-p (list) (list (list)))))
  (is (not (match-p (list 1 2 3) (list (list 1 2 3)))))
  (is (not (match-p (list 1 2 3) (list 1 2 3 4))))
  ;; Translatable
  (is (erlang-translatable-p (list)))
  (is (erlang-translatable-p (list (list 1 2 3) 4 5)))
  )

(test erlang-string
  ;; Matching strings
  (is (match-p "" ""))
  (is (match-p "hello" "hello"))
  ;; Mismatching strings
  (is (not (match-p "" "\n")))
  (is (not (match-p "hello" "helloo")))
  (is (not (match-p "hello" "Hello")))
  ;; Translatable
  (is (erlang-translatable-p ""))
  (is (erlang-translatable-p "TEST"))
  )
