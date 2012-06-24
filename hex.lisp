;;; ------------------------------------------------------------
;;; HEAT EXCHANGERS
;;; ------------------------------------------------------------
(defclass heat-exchanger (element)
  ((passage-1   :initarg :passage-1   :reader passage-1)
   (passage-2   :initarg :passage-2   :reader passage-2)))

(defgeneric exchange-area (heat-exchanger)
  (:documentation "Total area of heat exchange, with respect to fins-side."))

;;; - - - - - - -

(defun effectiveness-cross-unmixed (cmin/cmax ntu)
  "Effectiveness for a cross-flow heat exchanger, both fluids unmixed."
  (- 1
     (exp (* (/ 1 cmin/cmax)
	     (expt ntu 0.22)
	     (- (exp (- (* cmin/cmax (expt ntu 0.78))))
		1)))))


;;; ------------------------------------------------------------
;;; TUBES BANK HEAT EXCHANGERS
;;; ------------------------------------------------------------
(defclass tubes-bank-hex (heat-exchanger)
  ((rows            :initarg :rows            :reader rows)
   (circuit-step    :initarg :circuit-step    :reader circuit-step)
   ;; Below we define some initarg & reader aliases for code clarity:
   (passage-1       :initarg :tubes-passage   :reader tubes-passage)
   (passage-2       :initarg :fins-passage    :reader fins-passage)))

;;; - - - - - - -

(defmethod exchange-area ((hex tubes-bank-hex))
  (* (alpha (fins-passage hex))
     (area (face (fins-passage hex)))
     (rows hex)
     (row-pitch (fins-passage hex))))

(defmethod u-value ((hex tubes-bank-hex) (flv list))
  ;; Fins-side U-value of a tubes-bank heat exchanger
  (let* ((pas-fins (fins-passage hex))
	 (pas-tubes (tubes-passage hex))
	 (fl-fins (second flv))
	 (fl-tubes (first flv))
	 ;; flow in a single tube of the tubes bank
	 (fl-tube-1 (copy-flow fl-tubes
			       :mfr (/ (mfr fl-tubes)
				       (round (/ (height (face pas-fins))
						 (tube-pitch pas-fins)
						 (circuit-step hex))))))
	 ;; some geometric data
	 (din (diam (face pas-tubes)))
	 (dout (dout pas-fins))
	 (a-tubes/a-fins (* (/ din dout (- 1 (/ (fin-thick pas-fins)
						(fin-pitch pas-fins))))
			    (- 1 (af/at pas-fins))))
	 ;; heat resistances
	 (r-wall (/ (* din
		       (log (/ dout din)))
		    (* 2
		       (con (material pas-tubes) (tem (tubes-passage hex)))
		       (af/at pas-fins))))
	 (r-tubes (/ 1 (htc pas-tubes fl-tube-1) a-tubes/a-fins))
	 (r-fins (/ 1 (htc pas-fins fl-fins) (eta0 pas-fins fl-fins))))
    ;; Finally, compute U-value from heat resistances
    (/ 1 (+ r-wall r-tubes r-fins))))

;;; - - - - - - -

(defun q-tem-from-mean-flow! (hex flv-in flv-mean)
  ;; set the mean temperature of the passages
  (let ((tem-avg (apply #'avg (mapcar #'tem flv-mean))))
    (setf (tem (fins-passage hex)) tem-avg)
    (setf (tem (tubes-passage hex)) tem-avg))
  ;; calculate exchanged heat
  (let* ((cmin (reduce #'min (mapcar #'hcr flv-in)))
	 (cmax (reduce #'max (mapcar #'hcr flv-in)))
	 (qmax (* cmin
		  (abs (- (tem (first flv-in))
			  (tem (second flv-in))))))
	 (ntu (/ (* (u-value hex flv-mean)
		    (exchange-area hex))
		 cmin)))
    (* qmax
       (effectiveness-cross-unmixed (/ cmin cmax) ntu))))

;;; - - - - - - -

(defun q-tem! (hex flv-in)
  (q-tem-from-mean-flow! hex
			 flv-in
			 (flv-mean hex (first flv-in) (second flv-in))))

(defmethod output-flow! ((hex tubes-bank-hex) (flv-in list))
  (let* ((fl-water-in (first flv-in))
	 (fl-air-in (second flv-in))
	 (q (q-tem! hex flv-in))
	 (s (sign (- (tem fl-water-in)
		     (tem fl-air-in)))))
    (list (copy-flow fl-water-in
		     :tem (- (tem fl-water-in)
			     (/ q
				s
				(cp (fluid fl-water-in) (tem fl-water-in))
				(mfr fl-water-in))))
	  (copy-flow fl-air-in
		     :tem (- (tem fl-air-in)
			     (/ (- q)
				s
				(cp (fluid fl-air-in) (tem fl-air-in))
				(mfr fl-air-in)))))))

;;; - - - - - - -

(defun t-extreme (flw-in fla-in)
  (/ (+ (* (mfr flw-in)
	   (cp (fluid flw-in) (tem flw-in))
	   (tem flw-in))
	(* (mfr fla-in)
	   (cp (fluid fla-in) (tem fla-in))
	   (tem fla-in)))
     (+ (* (mfr flw-in)
	   (cp (fluid flw-in) (tem flw-in)))
	(* (mfr fla-in)
	   (cp (fluid fla-in) (tem fla-in))))))

(defun fl-water-mean (fl-water-in fl-air-in fl-air-mean)
  (let ((ta-mean (tem fl-air-mean))
	(t-extreme (t-extreme fl-water-in fl-air-in)))
    (copy-flow fl-water-in
	       :tem (find-root #'(lambda (tw-mean)
				   (+ (* (mfr fl-air-in)
					 (cp (fluid fl-air-in) ta-mean)
					 (- (tem fl-air-in)
					    ta-mean))
				      (* (mfr fl-water-in)
					 (cp (fluid fl-water-in) tw-mean)
					 (- (tem fl-water-in)
					    tw-mean))))
			       (tem fl-water-in)
			       t-extreme))))

(defun fl-air-mean (hex fl-water-in fl-air-in)
  (let ((t-extreme (t-extreme fl-water-in fl-air-in)))
    (copy-flow fl-air-in
	       :tem (find-root #'(lambda (ta-mean)
				   (let* ((fl-air-mean (copy-flow fl-air-in
								  :tem ta-mean))
					  (fl-water-mean (fl-water-mean fl-water-in
									fl-air-in
									fl-air-mean)))
				     (- (q-tem-from-mean-flow! hex
							       (list fl-water-in fl-air-in)
							       (list fl-water-mean fl-air-mean))
					(* 2
					   (mfr fl-air-in)
					   (cp (fluid fl-air-in) ta-mean)
					   (- (tem fl-air-in)
					      ta-mean)))))
			       (tem fl-air-in)
			       t-extreme))))

(defun flv-mean (hex fl-water-in fl-air-in)
  (let* ((fla (fl-air-mean hex fl-water-in fl-air-in))
	 (flw (fl-water-mean fl-water-in fl-air-in fla)))
    (list flw fla)))


;;; ------------------------------------------------------------
;;; CROSS FLOW HEAT EXCHANGERS
;;; ------------------------------------------------------------
(defclass cross-flow-hex (heat-exchanger)
  ())