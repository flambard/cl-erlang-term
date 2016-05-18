(in-package :erlang-term-trivia)

(defpattern binary (&rest segments)
  "Pattern matching for Erlang binaries.

 Example:

 (match (binary 1 2 88)
   ((binary 1 b \"X\") b))
 => 2
"
  `(class erlang-binary
          :bytes (vector
                  ,@(flatten-string-patterns-to-bytes segments))))

(defun flatten-string-patterns-to-bytes (patterns)
  (reduce #'(lambda (pattern acc)
              (if (stringp pattern)
                  (nconc (string-to-byte-list pattern) acc)
                  (cons pattern acc)))
          patterns
          :initial-value nil
          :from-end t))

(defpattern erlang-string (string)
"Pattern for matching \"Erlang strings\", lists of integers in the ascii range.
 Example:

 (match (list 104 101 108 108 111)
   ((erlang-string \"hello\") t))
 => t
"
  `(list ,@(string-to-byte-list string)))


(defpattern tuple (&rest patterns)
  "Pattern for matching Erlang tuples.

 Example:

 (match (tuple 1 2 3)
   ((tuple a b c) (+ a b c)))
 => 6"
  `(class erlang-tuple
          :elements (simple-vector ,@patterns)))

