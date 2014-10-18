(in-package :erlang-term-test)

(defclass mock-atom-cache ()
  ())

(defmethod etf-aci:get-atom ((reference integer) (cache mock-atom-cache))
  (values :abba t))

(defmethod etf-aci:get-atom ((reference (eql 0)) (cache mock-atom-cache))
  (values :|false| t))

(defmethod etf-aci:get-atom ((reference (eql 1)) (cache mock-atom-cache))
  (values :|true| t))

(defmethod etf-aci:get-atom ((reference (eql 19)) (cache mock-atom-cache))
  (values nil nil))


(defmethod etf-aci:put-atom ((atom symbol) (cache mock-atom-cache))
  42)

(defmethod etf-aci:put-atom ((atom (eql nil)) (cache mock-atom-cache))
  0)

(defmethod etf-aci:put-atom ((atom (eql t)) (cache mock-atom-cache))
  1)

(defmethod etf-aci:put-atom ((atom (eql :|false|)) (cache mock-atom-cache))
  2)

(defmethod etf-aci:put-atom ((atom (eql :|true|)) (cache mock-atom-cache))
  3)

(defmethod etf-aci:put-atom ((atom (eql :nope)) (cache mock-atom-cache))
  nil)
