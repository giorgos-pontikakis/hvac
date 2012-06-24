;;; ------------------------------------------------------------
;;; FLOWS
;;; ------------------------------------------------------------
(defclass flow ()
  ((fluid :initarg :fluid :reader fluid)
   (mfr   :initarg :mfr   :accessor mfr)
   (tem   :initarg :tem   :accessor tem)
   (pres  :initarg :pres  :accessor pres)))

(defun copy-flow (flow &key tem mfr pres)
  (let ((flcopy (make-instance 'flow
			       :fluid (fluid flow)
			       :mfr (mfr flow)
			       :tem (tem flow)
			       :pres (pres flow))))
    (when tem
      (setf (tem flcopy) tem))
    (when mfr
      (setf (mfr flcopy) mfr))
    (when pres
      (setf (pres flcopy) pres))
    flcopy))


;;; --- Functions for flows ---

(defun hcr (flow)
  "Heat capacity rate"
  (* (mfr flow)
     (cp (fluid flow) (tem flow))))


;;; --- Functions for flows in passages ---

(defgeneric tfilm (passage flow)
  (:documentation "Film temperature"))

(defmethod tfilm ((pas passage) (fl flow))
  (/ (+ (tem fl) (tem pas)) 2))

;;; - - - - - - -

(defgeneric vmean (passage flow)
  (:documentation "Mean velocity"))

(defmethod vmean ((pas passage) (fl flow))
  (/ (mfr fl)
     (area (face pas))
     (rho (fluid fl) (tem fl))))

;;; - - - - - - -

(defgeneric vmax (passage flow)
  (:documentation "Maximum velocity"))

(defmethod vmax ((tbank tubes-bank) (fl flow))
  (/ (vmean tbank fl)
     (sigma tbank)))

;;; - - - - - - -

(defgeneric re (passage flow)
  (:documentation "Reynolds number"))

(defmethod re ((pas conduit) (fl flow))
  (let ((tem (tem fl)))
    (/ (* (rho (fluid fl) tem)
	  (vmean pas fl)
	  (dhyd (face pas)))
       (vis (fluid fl) tem))))

(defmethod re ((tbank tubes-bank) (fl flow))
  (let ((tf (tfilm tbank fl)))
    (/ (* (rho (fluid fl) tf)
	  (vmax tbank fl)
	  (dhyd tbank))
       (vis (fluid fl) tf))))

;;; - - - - - - -

(defgeneric j-h (pas flow)
  (:documentation "Colburn dimensionless heat transfer coefficient"))

(defmethod j-h ((pas kays-cf-7.0-5/8j) (fl flow))
  (funcall (lambda-line 5000 0.0079 7000 0.0061) (re pas fl)))

(defmethod j-h ((pas kays-8.0-3/8t) (fl flow))
  (funcall (lambda-line 400 0.0151 4000 0.006) (re pas fl)))

(defmethod j-h ((pas kays-7.75-5/8t) (fl flow))
  (funcall (lambda-line 900 0.009 9000 0.004) (re pas fl)))

;;; - - - - - - -

(defgeneric nu (passage flow)
  (:documentation "Nusselt number"))

(defmethod nu ((pas conduit) (fl flow))
  (let ((re (re pas fl))
	(pr (pr (fluid fl) (tem fl))))
    (cond ((< re 2300) 4.36)
	  ((< re 5e6) (gnielinski re pr (ff pas fl)))
	  (t (error "Too high reynolds number!")))))

;;; - - - - - - -

(defgeneric htc (passage flow)
  (:documentation "Heat transfer coefficient"))

(defmethod htc ((pas body) (fl flow))
  (/ (* (j-h pas fl)
	(cp (fluid fl) (tfilm pas fl))
	(rho (fluid fl) (tfilm pas fl))
	(vmax pas fl))
     (expt (pr (fluid fl) (tfilm pas fl)) 2/3)))

(defmethod htc ((pas conduit) (fl flow))
  (/ (* (nu pas fl)
	(con (fluid fl) (tem fl)))
     (dhyd pas)))

;;; - - - - - - -

(defgeneric ff (passage flow)
  (:documentation "Friction factor"))

(defmethod ff ((dc duct) flow)
  (let ((re (re dc flow)))
    (cond ((< re 2300) (/ 64 re))
	  ((< re 5e6) (sq (find-root (colebrook (diam (face dc))
						re
						(rough dc))
				     1d-8 1d+8)))
	  (t (error "Too high reynolds number!")))))

(defmethod ff ((tb tube) flow)
  (let ((re (re tb flow)))
    (cond ((< re 2300) (/ 64 re))
	  ((< re 5e6) (haaland (diam (face tb))
			       re
			       (rough tb)))
	  (t (error "Too high reynolds number!")))))

;;; - - - - - - -

(defun eta-circular-fin (r-tube r-fin t-fin h k)
  "Circular fin efficiency for an adiabatic tip. Approximation
   from the book of McQuiston & Parker"
  (let* ((phi (* (- (/ r-fin r-tube)
		    1)
		 (+ 1
		    (* 0.35
		       (log (/ r-fin r-tube))))))
	 (m (sqrt (/ (* 2 h) (* k t-fin))))
	 (m-r-phi (* m r-tube phi)))
    (/ (tanh m-r-phi)
       m-r-phi)))


(defgeneric eta-fin (passage flow))

(defmethod eta-fin ((pas body) (fl flow))
  ;; The value is 1 by definition, except for finned passages, where
  ;; it is calculated with separate methods.
  1)

(defmethod eta-fin ((pas tubes-bank-continuous-fins) (fl flow))
  "Efficiency of continuous fins for a hexagonal array of finned tubes"
  ;; Fin equivalent diameter calculation according to the book of
  ;; McQuiston & Parker. Since the fin is continuous, an adiabatic tip
  ;; is assumed and eta-circular-fin is used directly.
  (let* ((r1 (/ (dout pas) 2))
	 (m1 (/ (tube-pitch pas) 2))
	 (l1 (/ (sqrt (+ (sq (row-pitch pas))
			 (sq m1)))
		2))
	 (l (max m1 l1))
	 (m (min m1 l1))
	 (beta (/ l m))
	 (psi (/ m r1))
	 (r2-eq (* 1.28 r1 psi (sqrt (- beta 0.2)))))
    (eta-circular-fin r1
		      r2-eq
		      (fin-thick pas)
		      (htc pas fl)
		      (con (material pas)
			   (tem fl)))))

(defmethod eta-fin ((pas tubes-bank-circular-fins) (fl flow))
  "Efficiency of circular fins for a bank of finned tubes"
  ;; In contrast to continuous fins, the corrections for an active tip
  ;; (convective heat transfer boundary condition) are applied before
  ;; calling eta-circular-fin. Not much difference...
  (let* ((tau (fin-thick pas))
	 (r1 (/ (dout pas) 2))
	 (r2 (/ (fin-diam pas) 2))
	 (r2c (+ r2 (/ tau 2))))
    (eta-circular-fin r1
		      r2c
		      tau
		      (htc pas fl)
		      (con (material pas) (tem fl)))))

;;; - - - - - - -

(defun eta0 (passage flow)
  "Overall surface efficiency wrt heat exchange"
  (- 1
     (* (af/at passage)
	(- 1 (eta-fin passage flow)))))

;;; - - - - - - -
