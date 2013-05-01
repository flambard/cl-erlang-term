(in-package :erlang-term-test)

(defclass mock-atom-cache ()
  ())

(defmethod etf-aci:get-atom ((reference integer) (cache mock-atom-cache))
  (values :abba t))

(defmethod etf-aci:get-atom ((reference (eql 19)) (cache mock-atom-cache))
  (values nil nil))


(defmethod etf-aci:put-atom ((atom symbol) (cache mock-atom-cache))
  42)

(defmethod etf-aci:put-atom ((atom (eql :nope)) (cache mock-atom-cache))
  nil)
