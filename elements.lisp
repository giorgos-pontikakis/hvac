;;; ------------------------------------------------------------
;;; ELEMENTS
;;; ------------------------------------------------------------
(defclass element ()
  ())

(defgeneric u-value (element flows)
  (:documentation
   "U-value of an element. For compact heat exchangers, it refers
    to the fins' side."))

(defgeneric output-flow! (element flows)
  (:documentation
   "Given an element and a flow or a flow-vector as input, it
    calculates the output (flow or flow-vector). Modifies the
    state of the element."))
