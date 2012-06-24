;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;;; Should I add make- functions for every class, like this?
;;; Currently, I do that for ducts, not for heat exchangers
(defun make-tubes-bank-hex (tubes-passage fins-passage rows circuit-step)
  (make-instance 'tubes-bank-hex
		 :tubes-passage tubes-passage
		 :fins-passage fins-passage
		 :rows rows
		 :circuit-step circuit-step))


;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;;; Should I include a passage list in the elements class and then have
;;; an accessor for each element that chooses from the list? E.g.:
(defclass element ()
  (passage-list))

(defclass heat-exchanger (element)
  ())

(defgeneric tubes-passage (tubes-bank-hex)
  (:documentation "Accessor for the first passage of the passage list"))

(defmethod tubes-passage ((hex heat-exchanger))
  (first (passage-list hex)))

(defgeneric fins-passage (tubes-bank-hex)
  (:documentation "Accessor for the second passage of the passage list"))

(defmethod tubes-passage ((hex heat-exchanger))
  (second (passage-list hex)))


;;; and then, for ducts:
(defgeneric passage (dc straight)
  (first (passage-list dc)))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
