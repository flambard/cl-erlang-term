(in-package :erlang-term-optima)

;;
;; Pattern for matching Erlang tuples.
;;
;; Syntax:
;;
;; tuple-pattern-constructor ::= (tuple PATTERN*)
;;
;;
;; Example:
;;
;; (match (tuple 1 2 3)
;;   ((tuple a b c) (+ a b c)))
;; => 6
;;

(defstruct (tuple-pattern
             (:include optima::constructor-pattern)
             (:constructor make-tuple-pattern (&rest optima::subpatterns)))
  )


(defmethod optima::destructor-equal ((x tuple-pattern) (y tuple-pattern))
  (= (optima::constructor-pattern-arity x)
     (optima::constructor-pattern-arity y)))

(defmethod optima::destructor-predicate-form ((pattern tuple-pattern) var)
  `(and (typep ,var 'erlang-tuple)
        (= (tuple-arity ,var) ,(optima::constructor-pattern-arity pattern))))

(defmethod optima::destructor-forms ((pattern tuple-pattern) var)
  (loop for i from 0 below (optima::constructor-pattern-arity pattern)
        collect `(tuple-ref ,var ,i)))

(defmethod optima::parse-constructor-pattern ((name (eql 'tuple)) &rest args)
  (apply #'make-tuple-pattern (mapcar #'optima::parse-pattern args)))

(defmethod optima::unparse-pattern ((pattern tuple-pattern))
  `(tuple ,@(mapcar #'optima::unparse-pattern
                    (tuple-pattern-subpatterns pattern))))
