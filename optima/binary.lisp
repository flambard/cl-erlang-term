(in-package :erlang-term-optima)

;;
;; Pattern matching for Erlang binaries.
;;
;; Syntax:
;;
;; binary-pattern-constructor ::= (binary SEGMENT*)
;;
;;
;; Example:
;;
;; (match (binary 1 2 88)
;;   ((binary 1 b "X") b))
;; => 2
;;


;;;
;;; BINARY pattern
;;;

(defstruct (binary-pattern
             (:include constructor-pattern)
             (:constructor make-binary-pattern (&rest subpatterns)))
  )


(defmethod constructor-pattern-destructor-sharable-p ((x binary-pattern)
                                                      (y binary-pattern))
  (= (constructor-pattern-arity x) (constructor-pattern-arity y)))

(defmethod constructor-pattern-make-destructor ((pattern binary-pattern) var)
  (make-destructor
   :predicate-form `(and (typep ,var 'erlang-binary)
                         (= (size ,var) ,(constructor-pattern-arity pattern)))
   :accessor-forms (loop
                      for i from 0 below (constructor-pattern-arity pattern)
                      collect `(aref (bytes ,var) ,i)) ))

(defmethod parse-constructor-pattern ((name (eql 'binary)) &rest args)
  (apply #'make-binary-pattern
         (mapcar #'parse-pattern
                 (flatten-string-patterns-to-bytes args))))

(defmethod unparse-pattern ((pattern binary-pattern))
  ;; Currently strings in patterns will not be unparsed back to strings,
  ;; they will be unparsed to constant bytes.
  `(binary ,@(mapcar #'unparse-pattern (binary-pattern-subpatterns pattern))))


;;;
;;; Helper functions
;;;

(defun flatten-string-patterns-to-bytes (patterns)
  (reduce #'(lambda (pattern acc)
              (if (stringp pattern)
                  (nconc (string-to-byte-list pattern) acc)
                  (cons pattern acc)))
          patterns
          :initial-value nil
          :from-end t))
