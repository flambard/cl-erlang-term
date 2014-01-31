(in-package :erlang-term-optima)

;;
;; Derived pattern for matching "Erlang strings", lists of integers in the ascii
;; range.
;;
;; Syntax:
;;
;; (erlang-string STRING)
;;
;;
;; Example:
;;
;; (match (list 104 101 108 108 111)
;;   ((erlang-string "hello") t))
;; => t
;;

(defpattern erlang-string (string)
  `(list ,@(map 'list #'char-code string)))
