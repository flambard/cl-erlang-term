(in-package :erlang-term-test)

(in-suite optima-patterns)

(test binary-pattern

  (is-true (match (binary)
             ((binary 0) nil)
             ((binary 1 2 3) nil)
             ((binary) t)))

  (is-true (match (binary 255 255 255 0)
             ((binary) nil)
             ((binary 255 255 255) nil)
             ((binary 255 255 255 0 0) nil)
             ((binary a b c d)
              (and (= 255 a b c) (= 0 d)))))

  (is-true (match (string-to-binary "hej")
             ((binary) nil)
             ((binary 104 101 106 33) nil)
             ((binary 104 101) nil)
             ((binary 104 101 106) t)))

  (is-true (match (binary (char-code #\X))
             ((binary 0 "X" 34) nil)
             ((binary "X" 0) nil)
             ((binary "X") t)))

  (is-true (match (binary 104 101 106)
             ((binary) nil)
             ((binary "hej!") nil)
             ((binary "he") nil)
             ((binary "hej") t)))

  )


(test erlang-string-pattern
  (is-true (match (list)
             ((erlang-string "hello") nil)
             ((erlang-string "") t)))
  (is-true (match (list 104 101 108 108 111)
             ((list 1 2 3) nil)
             ((erlang-string "") nil)
             ((erlang-string "hello") t)))
  )

(test tuple-pattern
  (is-true (match (tuple)
             ((tuple x) x)
             ((tuple 1 2) nil)
             ((tuple) t)))
  (is-true (match (tuple 1 2 3)
             ((tuple 1 2 2) nil)
             ((tuple) nil)
             ((tuple a b) (+ a b))
             ((tuple a b c d) (+ a b c d))
             ((tuple 1 2 3) t)))
  (is-true (match (tuple 1 2 3)
             ((tuple a b) (+ a b))
             ((tuple a b c d) (+ a b c d))
             ((tuple a b c)
              (progn a b c t))))
  (is-true (match (tuple (list 1 2) (tuple 3 4) 5)
             ((tuple (list a b) (tuple c d) 5)
              (progn a b c d t))))
  )
