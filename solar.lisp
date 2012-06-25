(in-package :hvac)

(declaim (optimize (speed 0) (space 0) (debug 3)))

(defclass place ()
  ((lon   :accessor lon   :initarg :lon)
   (lat   :accessor lat   :initarg :lat)
   (tz    :accessor tz    :initarg :tz)
   (tau-b :accessor tau-b :initarg :tau-b)
   (tau-d :accessor tau-d :initarg :tau-d)))

(defparameter *solar-constant* 1367
  "W/m2")

(defun n-day (month day)
  "Number of the day from the beginning of the year"
  (let ((month-day-table '((:jan 31)
			   (:feb 28)
			   (:mar 31)
			   (:apr 30)
			   (:may 31)
			   (:jun 30)
			   (:jul 31)
			   (:aug 31)
			   (:sep 30)
			   (:oct 31)
			   (:nov 30)
			   (:dec 31))))
    (iter (for (m d) in month-day-table)
	  (until (eql m month))
	  (sum d into total)
	  (finally (return (+ day total))))))

(defun et (n-day)
  "Equation of time, returns the difference in minutes between the
   mean time [clock time] and the apparent solar time"
  (let ((gamma (* 360 (/ (1- n-day) 365))))
    (* 2.2918 (+ 0.0075
		 (* +0.1868 (cos gamma))
		 (* -3.2077 (sin gamma))
		 (* -1.4615 (cos (* 2 gamma)))
		 (* -4.0890 (sin (* 2 gamma)))))))

(defun ast (place month day lst)
  "Return hour as decimal "
  (let ((lon (lon place))
	(lsm (* 15 (tz place)))
	(et (et (n-day month day))))
    (+ lst
       (/ et 60)
       (/ (- lon lsm) 15))))


(defun sol-declination (month day)
  "Return angle in degrees"
  (let ((n (n-day month day)))
    (* 23.45
       (sin (* 2 pi
	       (/ (+ n 284)
		  365))))))

(defun hour-angle (ast)
  (* 15 (- ast 12)))

(defun sol-altitude (place month day lst)
  "Return solar altitude in degrees"
  (let ((lat (deg-to-rad (lat place)))
	(dec (deg-to-rad (sol-declination month day)))
	(h (deg-to-rad (hour-angle (ast place month day lst)))))
    (rad-to-deg
     (asin (+ (* (cos lat) (cos dec) (cos h)) 
	      (* (sin lat) (sin dec)))))))

(defun sol-azimuth (place month day lst)
  (let ((lat (deg-to-rad (lat place)))
	(alt (deg-to-rad (sol-altitude place month day lst)))
	(dec (deg-to-rad (sol-declination month day)))
	(h (deg-to-rad (hour-angle (ast place month day lst))))) 
    (list (/ (* (sin h) (cos dec))
	     (cos alt))
	  (/ (- (* (cos h) (cos dec) (sin lat))
		(* (sin dec) (cos lat)))
	     (cos alt)))))

(defun deg-to-rad (deg)
  (/ (* 2 pi deg) 360))

(defun rad-to-deg (rad)
  (/ (* rad 360) (* 2 pi)))

(defun air-mass (sol-alt)
  (/ 1
     (+ (sin (deg-to-rad sol-alt))
	(* 0.50572 (expt (+ 6.07995 sol-alt) -1.6364)))))


(defun irr-extraterrestrial (month day)
  (let ((n (n-day month day)))
    (* *solar-constant*
       (+ 1
	  (* 0.033
	     (cos (* 2 pi (/ (- n 3) 365))))))))

(defun air-mass-ab (place)
  (let ((tau-b (tau-b place))
	(tau-d (tau-d place)))
    (+ 1.219
       (* -0.043 tau-b)
       (* -0.151 tau-d)
       (* -0.204 tau-b tau-d))))

(defun air-mass-ad (place)
  (let ((tau-b (tau-b place))
	(tau-d (tau-d place)))
    (+ 0.202
       (* +0.852 tau-b)
       (* -0.007 tau-d)
       (* -0.357 tau-b tau-d))))

(defun irr-beam (place month day lst)
  (let ((e0 (irr-extraterrestrial month day))
	(m (air-mass (sol-altitude place month day lst)))
	(ab (air-mass-ab place)))
    (* e0
       (exp (* -1 (tau-b place) (expt m ab))))))

(defun irr-diff (place month day lst)
  (let ((e0 (irr-extraterrestrial month day))
	(m (air-mass (sol-altitude place month day lst)))
	(ad (air-mass-ad place)))
    (* e0
       (exp (* -1 (tau-d place) (expt m ad))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SURFACES

(defun cos-incidence (place month day lst azimuth tilt)
  (let* ((phi (asin (first (sol-azimuth place month day lst)))) 
	 (gamma (- phi (deg-to-rad azimuth)))
	 (beta (deg-to-rad (sol-altitude place month day lst )))
	 (sigma (deg-to-rad tilt))) 
    (+ (* (cos beta) (cos gamma) (sin sigma))
       (* (sin beta) (cos sigma)))))

(defun surface-irr-beam (place month day lst azimuth tilt)
  (let ((irr-beam (irr-beam place month day lst))
	(cos-theta (cos-incidence place month day lst azimuth tilt)))
    (* cos-theta irr-beam)))

(defun surface-irr-diff (place month day lst azimuth tilt)
  (let* ((irr-diff (irr-diff place month day lst))
	 (cos-theta (cos-incidence place month day lst azimuth 90))
	 (y (max 0.45 (+ 0.55 (* 0.437 cos-theta) (* 0.313 (expt cos-theta 2)))))
	 (sigma (deg-to-rad tilt)))
    (break)
    (if (<= tilt 90)
	(* irr-diff
	   (+ (* y (sin sigma))
	      (cos sigma)))
	(* irr-diff y (sin sigma)))))

(defun surface-irr-ground (place month day lst tilt)
  (let ((irr-beam (irr-beam place month day lst))
	(irr-diff (irr-diff place month day lst))
	(ground-reflectance 0.2)
	(beta (deg-to-rad (sol-altitude place month day lst)))
	(sigma (deg-to-rad tilt)))
    (* (+ (* irr-beam (sin beta))
	  irr-diff)
       ground-reflectance
       (* 0.5 (- 1 (cos sigma))))))
