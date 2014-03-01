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
             (:include constructor-pattern)
             (:constructor make-tuple-pattern (&rest subpatterns)))
  )


(defmethod constructor-pattern-destructor-sharable-p ((x tuple-pattern)
                                                      (y tuple-pattern))
  (= (constructor-pattern-arity x) (constructor-pattern-arity y)))

(defmethod constructor-pattern-make-destructor ((pattern tuple-pattern) var)
  (make-destructor
   :predicate-form `(and (typep ,var 'erlang-tuple)
                         (= (tuple-arity ,var)
                            ,(constructor-pattern-arity pattern)))
   :accessor-forms (loop
                      for i from 0 below (constructor-pattern-arity pattern)
                      collect `(tuple-ref ,var ,i))))

(defmethod parse-constructor-pattern ((name (eql 'tuple)) &rest args)
  (apply #'make-tuple-pattern (mapcar #'parse-pattern args)))

(defmethod unparse-pattern ((pattern tuple-pattern))
  `(tuple ,@(mapcar #'unparse-pattern (tuple-pattern-subpatterns pattern))))
