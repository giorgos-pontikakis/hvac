(defparameter dc (make-instance 'duct
				:name 'straight
				:rough 9d-5
				:face (make-instance 'circle :diam 0.6)))

(defparameter fl (make-instance 'flow
				:fluid *air*
				:mfr (/ 16000 3600 1.2)
				:tem (kelvin 20)
				:pres +patm+))

(defparameter tbank (make-instance 'tubes-bank
				   :face (make-instance 'rectangle
							:width 0.5
							:height 0.4)
				   :dout (* 0.676 25.4e-3)
				   :row-pitch (* 1.75 25.4e-3)
				   :tube-pitch (* 1.5 25.4e-3)
				   :num-rows 3
				   :sigma 0.481))

(defparameter tb (make-instance 'tubes-bank
				:face (make-instance 'rectangle
						     :width 0.5
						     :height 0.4)
				:dout (* .676 25.4e-3)
				:row-pitch (* 1.75 25.4e-3)
				:tube-pitch (* 1.5 25.4e-3)
				:num-rows 3))


(defparameter ftb (make-instance 'finned-tubes-bank
				 :face (make-instance 'rectangle
						      :width 0.5
						      :height 0.4)
				 :dout (* 5/8 25.4e-3)
				 :row-pitch (* 1.75 25.4e-3)
				 :tube-pitch (* 1.5 25.4e-3)
				 :num-rows 3
				 :fin-thick (* 0.016 25.4e-3)
				 :fin-pitch 1/305))

(defparameter ftb2 (make-instance 'finned-tubes-bank
				 :face (make-instance 'rectangle
						      :width 0.5
						      :height 0.4)
				 :dout (* 5/8 25.4e-3)
				 :row-pitch (* 1.75 25.4e-3)
				 :tube-pitch (* 1.5 25.4e-3)
				 :num-rows 3
				 :fin-thick (* 0.016 25.4e-3)
				 :fin-pitch 1/305
				 :sigma 0.481
				 :alpha 554
				 :af/at 0.95))


(defparameter cf-7.0-5/8t (make-instance 'tubes-bank-circular-fins
					 :tem 700
					 :face (make-instance 'rectangle
							      :width 1.0
							      :height 0.2)
					 :dout 16.4e-3
					 :dhyd 6.68e-3
					 :row-pitch 31.3e-3
					 :tube-pitch 34.3e-3
					 :num-rows 3
					 :fin-thick 0.254e-3
					 :fin-pitch 1/275
					 :fin-diam 28.5e-3
					 :sigma 0.449
					 :alpha 269
					 :af/at 0.830))












(defparameter fl-mcquiston (make-instance 'flow
				       :fluid *air*
				       :mfr (* 1.2 (* 5.08 (* (to-si 2 "ft")
							      (to-si 1 "ft"))))
				       :tem (to-unit 70 "F" "K")
				       :pres +patm+))

(defparameter fl-water-mcquiston (make-instance 'flow
						:fluid *water*
						:mfr 0.1518
						:tem (to-unit 145 "F" "K")
						:pres +patm+))



(defparameter tube-mcquiston (make-instance 'tube
					    :face (make-instance 'circle
								 :diam (0.5 "inch"))
					    :tem nil
					    :material *aluminum*
					    :rough 0.d0))





(defparameter cf-7.0-5/8t-def (make-instance 'tubes-bank-circular-fins
					     :tem 700
					     :face (make-instance 'rectangle
								  :width 1.0
								  :height 0.2)
					     :dout 16.4e-3
					     :row-pitch 31.3e-3
					     :tube-pitch 34.3e-3
					     :num-rows 3
					     :fin-thick 0.254e-3
					     :fin-pitch 1/275
					     :fin-diam 28.5e-3))

;; example from incorpera, section 11.6
(defparameter pas1 (make-instance 'kays-cf-7.0-5/8j
				  :material *aluminum*
				  :tem 700
				  :face (make-instance 'rectangle
						       :width 0.1727364
						       :height 1.1578336)))
(defparameter pas2 (make-instance 'tube-conduit
				  :face (make-instance 'circle
						       :diam 13.8e-3)
				  :tem nil
				  :material *aluminum*
				  :rough 0.d0))

(defparameter hex (make-compact-heat-exchanger pas2 pas1 5 1))


(defparameter fl-water (make-instance 'flow
					   :fluid *water*
					   :mfr 1.0
					   :tem 290
					   :pres +patm+))

(defparameter fl-air (make-instance 'flow
				    :fluid *air*
				    :mfr 1.25
				    :tem 825
				    :pres +patm+))

(defparameter fl-water-mean (make-instance 'flow
					   :fluid *water*
					   :mfr 1.0
					   :tem 330
					   :pres +patm+))

(defparameter fl-air-mean (make-instance 'flow
					 :fluid *air*
					 :mfr 1.25
					 :tem 700
					 :pres +patm+))



(defparameter flv
  (list fl-water fl-air))

(defparameter flv-mean
  (list fl-water-mean fl-air-mean))