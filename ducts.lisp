;;; ------------------------------------------------------------
;;; DUCT PARTS
;;; ------------------------------------------------------------
(defclass duct-part (element)
  ((passages :initarg :passages :reader passages)
   (thick    :initarg :thick    :reader thick)))


(defclass straight (duct-part)
  ((length :initarg :len :reader len)))

(defclass elbow (duct-part)
  ((deg    :initarg :deg    :reader deg)
   (r-mean :initarg :r-mean :reader r-mean)))

(defclass rect-to-circ (straight)
  ())

(defun make-straight (face length)
  (make-instance 'straight
		 :len length
		 :passages (make-instance 'duct-conduit :face face)))

(defun make-elbow (face1 r-mean &key face2 (deg 90))
  (let ((pas (if (null face2)
		 (make-list 2 :initial-element (make-instance 'duct-conduit
							      :face face1))
		 (list (make-instance 'duct-conduit :face face1)
		       (make-instance 'duct-conduit :face face2)))))
    (make-instance 'elbow
		   :deg deg
		   :r-mean r-mean
		   :passages pas)))

(defun make-rect-to-circ (face1 face2 length)
  (make-instance 'rect-to-circ
		 :len length
		 :passages (list (make-instance 'duct-conduit :face face1)
				 (make-instance 'duct-conduit :face face2))))

;;; - - - - - - -

(defun dp-straight (el fl)
  (let ((pas (passages el)))
    (+ (ff pas fl)
       (/ (len pas)
	  (dhyd pas))
       (* (rho (fluid fl) (tem fl))
	  (sq (vmean pas fl))))))

(defun make-duct-output-flow (el fl pres-factor)
  (copy-flow fl :pres (- (pres fl)
			 (* pres-factor (dp-straight el fl)))))

;;; - - - - - - -

(defmethod output-flow! ((el straight) (fl flow))
  (make-duct-output-flow el fl 1))

(defmethod output-flow! ((el elbow) (fl flow))
  (make-duct-output-flow el fl 3))

(defmethod output-flow! ((el rect-to-circ) (fl flow))
  (make-duct-output-flow el fl 1.5))


;;; ------------------------------------------------------------
;;; COMPRESSORS
;;; ------------------------------------------------------------
(defclass compressor (element)
  ())

(defclass fan-10/10 (compressor)
  ())
