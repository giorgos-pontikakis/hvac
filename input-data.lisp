;;; ------------------------------------------------------------
;;; DEFINITIONS OF HEAT EXCHANGERS
;;; ------------------------------------------------------------
(defparameter *hex-incorpera*
  (make-instance 'tubes-bank-hex
		 :rows 5
		 :circuit-step 1
		 :fins-passage (make-instance 'kays-cf-7.0-5/8j
					      :material *aluminum*
					      :tem 700
					      :face (make-instance 'rectangle
								   :width 0.1727364
								   :height 1.1578336))
		 :tubes-passage (make-instance 'tube-conduit
					       :face (make-instance 'circle
								    :diam 13.8e-3)
					       :tem nil
					       :material *aluminum*
					       :rough 0.d0)))

(defparameter *hex0*
  (make-kays-7.75-5/8-hex 0.1727364 1.1578336 5))

(defparameter *hex1*
  (make-kays-8.0-3/8-hex 0.1727364 1.1578336 5))

(defparameter *kilkis-1.1*
  (make-instance 'tubes-bank-hex
		 :rows 2
		 :circuit-step 1
		 :fins-passage (make-instance 'kays-7.75-5/8t
					      :material *aluminum*
					      :tem nil
					      :face (make-instance 'rectangle
								   :width 0.5
								   :height 0.4))
		 :tubes-passage (make-instance 'tube-conduit
					       :face (make-instance 'circle
								    :diam (to-si 5/8 "inch"))
					       :tem nil
					       :material *aluminum*
					       :rough 0.d0)))

(defparameter *kilkis-1.1-b*
  (make-kays-7.75-5/8-hex 0.5 0.4 2))

(defparameter *vasiliadis-1*
  (make-kays-7.75-5/8-hex 1.2 1 2))

(defparameter *vasiliadis-2*
  (make-kays-8.0-3/8-hex 1.2 1 2))




;;; ------------------------------------------------------------
;;; DEFINITIONS OF FLOW VECTORS
;;; ------------------------------------------------------------
(defparameter *flv-incorpera*
  (list
   ;; water flow
   (make-instance 'flow
		  :fluid *water*
		  :mfr 1.0
		  :tem 290
		  :pres +patm+)
   ;; air flow
   (make-instance 'flow
		  :fluid *air*
		  :mfr 1.25
		  :tem 825
		  :pres +patm+)))

(defparameter *flv-kilkis-1.1*
  (list
   ;; water flow
   (make-instance 'flow
		  :fluid *water*
		  :mfr (* (rho *water* (to-si 80 "C")) (to-si 820 "lit/h"))
		  :tem (to-si 80 "C")
		  :pres +patm+)
   ;; air flow
   (make-instance 'flow
		  :fluid *air*
		  :mfr (* (rho *air* (to-si 14 "C")) (to-si 2400 "m3/h"))
		  :tem (to-si 14 "C")
		  :pres +patm+)))


(defparameter *flv-vasiliadis*
  (list
   ;; water flow
   (make-instance 'flow
		  :fluid *water*
		  :mfr (/ 15000 3600) ;; (* (rho *water* (to-si 80 "C")) (to-si 820 "lit/h"))
		  :tem (to-si 80 "C")
		  :pres +patm+)
   ;; air flow
   (make-instance 'flow
		  :fluid *air*
		  :mfr (* (rho *air* (to-si 5 "C")) (to-si 11200 "m3/h"))
		  :tem (to-si -5 "C")
		  :pres +patm+)))


;;; ------------------------------------------------------------
;;; KILKIS

(defparameter *hex-kilkis-12.2*
  (make-kays-7.75-5/8-hex 0.5 0.4 2 1))

(defparameter *hex-kilkis-12.2-b*
  (make-kays-8.0-3/8-hex 0.5 0.4 1 2))

(defparameter *flv-kilkis-12.2*
  (list
   ;; water flow
   (make-instance 'flow
		  :fluid *water*
		  :mfr (* (rho *water* (to-si 80 "C")) (to-si 1400 "lit/h"))
		  :tem (to-si 80 "C")
		  :pres +patm+)
   ;; air flow
   (make-instance 'flow
		  :fluid *air*
		  :mfr (* (rho *air* (to-si 14 "C")) (to-si 2850 "m3/h"))
		  :tem (to-si 14 "C")
		  :pres +patm+)))



(defparameter *flv-kilkis-12.2-b*
  (list
   ;; water flow
   (make-instance 'flow
		  :fluid *water*
		  :mfr (* (rho *water* (to-si 80 "C")) (to-si 700 "lit/h"))
		  :tem (to-si 80 "C")
		  :pres +patm+)
   ;; air flow
   (make-instance 'flow
		  :fluid *air*
		  :mfr (* (rho *air* (to-si 14 "C")) (to-si 2850 "m3/h"))
		  :tem (to-si 14 "C")
		  :pres +patm+)))



;;; ------------------------------------------------------------
;;; TESTING

(defparameter *flv*
  (list
   ;; water flow
   (make-instance 'flow
		  :fluid *water*
		  :mfr (* (rho *water* (to-si 80 "C"))
			  (to-si 1200 "lit/h"))
		  :tem (to-si 80 "C")
		  :pres +patm+)
   ;; air flow
   (make-instance 'flow
		  :fluid *air*
		  :mfr (* (rho *air* (to-si 0 "C"))
			  (to-si 2700 "m3/h"))
		  :tem (to-si 0 "C")
		  :pres +patm+)))

(defparameter *flv-b*
  (list
   ;; water flow
   (make-instance 'flow
		  :fluid *water*
		  :mfr (* (rho *water* (to-si 80 "C"))
			  (to-si 1200 "lit/h"))
		  :tem (to-si 80 "C")
		  :pres +patm+)
   ;; air flow
   (make-instance 'flow
		  :fluid *air*
		  :mfr (* (rho *air* (to-si 0 "C"))
			  (to-si 4500 "m3/h"))
		  :tem (to-si 0 "C")
		  :pres +patm+)))

(defparameter *flv-c*
  (list
   ;; water flow
   (make-instance 'flow
		  :fluid *water*
		  :mfr (* (rho *water* (to-si 80 "C"))
			  (to-si 2000 "lit/h"))
		  :tem (to-si 80 "C")
		  :pres +patm+)
   ;; air flow
   (make-instance 'flow
		  :fluid *air*
		  :mfr (* (rho *air* (to-si 0 "C"))
			  (to-si 4500 "m3/h"))
		  :tem (to-si 0 "C")
		  :pres +patm+)))

(defparameter *flv-d*
  (list
   ;; water flow
   (make-instance 'flow
		  :fluid *water*
		  :mfr (* (rho *water* (to-si 90 "C"))
			  (to-si 1200 "lit/h"))
		  :tem (to-si 90 "C")
		  :pres +patm+)
   ;; air flow
   (make-instance 'flow
		  :fluid *air*
		  :mfr (* (rho *air* (to-si 0 "C"))
			  (to-si 4500 "m3/h"))
		  :tem (to-si 0 "C")
		  :pres +patm+)))


(defparameter *hex-3/8*
  (make-kays-8.0-3/8-hex 0.7 0.5 4))

(defparameter *hex-3/8-b*
  (make-kays-8.0-3/8-hex 0.7 0.5 5))

(defparameter *hex-5/8*
  (make-kays-7.75-5/8-hex 0.7 0.5 4))

(defparameter *hex-5/8-b*
  (make-kays-7.75-5/8-hex 0.7 0.5 5))

(defparameter *hex-5/8-bb*
  (make-kays-7.75-5/8-hex 0.7 0.5 5 1))


;;; ------------------------------------------------------------

(defparameter *hex-5/8*
  (make-kays-7.75-5/8-hex 0.9 0.6 4))

(defparameter *hex-3/8*
  (make-kays-8.0-3/8-hex  0.8 0.5 3))

(defparameter *flv*
  (list
   ;; water flow
   (make-instance 'flow
		  :fluid *water*
		  :mfr (* (rho *water* (to-si 80 "C"))
			  (to-si 3500 "lit/h"))
		  :tem (to-si 80 "C")
		  :pres +patm+)
   ;; air flow
   (make-instance 'flow
		  :fluid *air*
		  :mfr (* (rho *air* (to-si 0 "C"))
			  (to-si 8000 "m3/h"))
		  :tem (to-si 0 "C")
		  :pres +patm+)))



(defparameter *tube1* (make-instance 'tube
				     :face (make-instance 'circle
							  :diam (to-si 5/8 "inch"))
				     :material *aluminum*
				     :tem (to-si 70 "C")
				     :rough 0))

(defparameter *tube22* (make-instance 'tube
				      :face (make-instance 'circle
							   :diam 22e-3)
				      :material *aluminum*
				      :tem (to-si 70 "C")
				      :rough 0))

(defparameter *tube28* (make-instance 'tube
				      :face (make-instance 'circle
							   :diam 28e-3)
				      :material *aluminum*
				      :tem (to-si 70 "C")
				      :rough 0))
