(defun make-kays-8.0-3/8-hex (width height num-rows &optional circuit-step)
  (let ((tubes-face (make-instance 'circle
				   :diam (to-si 3/8 "inch")))
	(fins-face (make-instance 'rectangle
				  :width width
				  :height height))
	(step (or circuit-step
		  (if (oddp num-rows) 2 1))))
    (make-instance 'tubes-bank-hex
		   :tubes-passage (make-instance 'tube
						 :face tubes-face
						 :material *aluminum*
						 :rough 0.d0)
		   :fins-passage  (make-instance 'kays-8.0-3/8t
						 :face fins-face
						 :material *aluminum*)
		   :rows num-rows
		   :circuit-step step)))


(defun make-kays-7.75-5/8-hex (width height num-rows &optional circuit-step)
  (let ((tubes-face (make-instance 'circle
				   :diam (to-si 5/8 "inch")))
	(fins-face (make-instance 'rectangle
				  :width width
				  :height height))
	(step (or circuit-step
		  (if (oddp num-rows) 2 1))))
    (make-instance 'tubes-bank-hex
		   :tubes-passage (make-instance 'tube
						 :face tubes-face
						 :material *aluminum*
						 :rough 0.d0)
		   :fins-passage  (make-instance 'kays-7.75-5/8t
						 :face fins-face
						 :material *aluminum*)
		   :rows num-rows
		   :circuit-step step)))

(defun solve-hex! (el flv)
  (let ((q-exchanged (q-tem! el flv))
	(tem-out (mapcar #'tem (output-flow! el flv))))
    (format t "Exchanged heat: ~,1f [kW] or ~,1f [kcal/h] ~&"
	    (to-unit q-exchanged "W" "kW")
	    (to-unit q-exchanged "W" "kcal/h"))
    (format t "Output temperatures: ~& Flow 1: ~,1f [oC]~& Flow 2: ~,1f [oC]"
	    (to-unit (first tem-out) "K" "C")
	    (to-unit (second tem-out) "K" "C"))
    (list q-exchanged
	  (first tem-out)
	  (second tem-out))))
