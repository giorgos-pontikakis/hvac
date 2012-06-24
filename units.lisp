(defparameter *si-units* (make-hash-table :test 'equal))
(defparameter *customary-units* (make-hash-table :test 'equal))
(defparameter *conversion-scales* (make-hash-table :test 'equal))
(defparameter *conversion-displacements* (make-hash-table :test 'equal))
(defparameter *prefixes*
  (make-table '(("E" .  1e+18)
		("P" .  1e+15)
		("T" .  1e+12)
		("G" .  1e+9)
		("M" .  1e+6)
		("k" .  1e+3)
		("h" .  1e+1)
		("d" .  1e-1)
		("c" .  1e-2)
		("m" .  1e-3)
		("μ" .  1e-6) ;; does this work everywhere?
		("n" .  1e-9)
		("p" .  1e-12)
		("f" .  1e-15)
		("a" .  1e-18))
	      :test 'equal))


;;; ------------------------------------------------------------
;;; DIMENSIONS AND UNITS
;;; ------------------------------------------------------------

;;; --- Utilities for dimension vectors ---

(defun dim* (&rest dims)
  (if (null dims)
      nil
      (reduce #'mat+ dims)))

(defun dim/ (&rest dims)
  (if (null dims)
      nil
      (reduce #'mat- dims)))

(defun dim-expt (dim power)
  (mat* power dim))

(defun dim-equal (dim1 dim2)
  (equalp dim1 dim2))


;;; --- Utilities for named units (registered in the tables) ---

(defun dim (name)
  (or (entry *si-units*        name)
      (entry *customary-units* name)
      (error "Unit not found: ~A" name)))

(defun scale (name)
  (or (entry *conversion-scales* name)
      (error "Unit not found: ~A" name)))

(defun displacement (name)
  (entry *conversion-displacements* name))

(defun registered-p (name)
  (or (key-exists-p *si-units* name)
      (key-exists-p *customary-units* name)))


;;; --- Parsing units ---

(defun split-unit (unit)
  (mapcar #'(lambda (prod)
	      (split-sequence #\. prod))
	  (split-sequence #\/ unit)))

(defun parse-power (unit)
  (let ((num-pos (or (position #\+ unit)
		     (position #\- unit)
		     (position-if #'digit-char-p unit))))
    (values (subseq unit 0 num-pos)
	    (if (null num-pos)
		1
		(parse-integer (subseq unit num-pos))))))

(defun parse-prefix (unit)
  (let ((prefix (subseq unit 0 1))
	(name (subseq unit 1)))
    (if (and (key-exists-p *prefixes* prefix)
	     (registered-p name))
	(values name (entry *prefixes* prefix))
	(values unit 1))))

(defun parse-splitted-unit (splitted-unit div-fn mult-fn prefix-power-fn)
  (reduce div-fn
	  (mapcar #'(lambda (product)
		      (reduce mult-fn
			      (mapcar prefix-power-fn product)))
		  splitted-unit)))

(defun parse-dim (unit)
  (parse-splitted-unit (split-unit unit)
		       #'dim/
		       #'dim*
		       #'(lambda (unit)
			   (mvb (name power) (parse-power unit)
				(dim-expt (dim (parse-prefix name)) power)))))

(defun parse-scale (unit)
  (parse-splitted-unit (split-unit unit)
		       #'/
		       #'*
		       #'(lambda (unit)
			   (mvb (name power) (parse-power unit)
				(mvb (no-prefix-name prefix-scale) (parse-prefix name)
				     (expt (* prefix-scale
					      (scale no-prefix-name))
					   power))))))


(defun parse-displacement (unit)
  (or (displacement unit) 0))


;;; --- Unit names ---

(defun registered-si-name (dim)
  (iter (for i in (key *si-units* dim #'dim-equal))
	(if (= (scale i) 1)
	    (return i))))

(defun fundamental-si-name (dim)
  (if (null dim)
      nil
      (flet ((make-fundamental-dim (n len)
	       (let ((dim (make-array len :initial-element 0)))
		 (setf (aref dim n) 1)
		 dim)))
	(let* ((len (length dim))
	       (lst (iter (for pow in-vector dim)
			  (for j index-of-sequence dim)
			  (for fu = (make-fundamental-dim j len))
			  (if (not (= 0 pow))
			      (collect (concatenate 'string
						    (registered-si-name fu)
						    (if (not (= 1 pow))
							(write-to-string pow))))))))
	  (reduce #'(lambda (arg1 arg2)
		      (concatenate 'string arg1 "." arg2))
		  lst)))))

(defun si-name (unit)
  (let ((dim (parse-dim unit)))
    (or (registered-si-name dim)
	(fundamental-si-name dim))))


;;; --- Unit tables ---

;;; Each table line should be a list of: (unit1 unit2 scale displacement).
;;;
;;; Default scale = 1
;;; Default displacement = 0
;;;
;;; The coefficients are entered so that:
;;; scale * (x1[unit1] + displacement) = x2[unit2]

(defun define-unit-table (table list)
  (dolist (sublist list)
    (destructuring-bind (a b &optional (c 1) (d 0)) sublist
      ;; Check for already registered keys
      (if (registered-p a)
	  (error "This unit is already defined: ~A" a))
      ;; handle dimension tables
      (if (stringp b)
	  (setf (entry table a) (parse-dim b))
	  (setf (entry table a) b))
      ;; handle conversion table
      (if (stringp b)
	  (setf (entry *conversion-scales* a) (* c (parse-scale b)))
	  (setf (entry *conversion-scales* a) c))
      ;; handle displacement table
      (if (stringp b)
	  (setf (entry *conversion-displacements* a) (+ d (/ (parse-displacement b) c)))
	  (setf (entry *conversion-displacements* a) d)))))

;;; ------------------------------------------------------------
;;; UNITS DEFINITIONS
;;; ------------------------------------------------------------
(define-unit-table *si-units*
    '(("kg"      #(1   0   0   0   0   0   0))
      ("m"       #(0   1   0   0   0   0   0))
      ("s"       #(0   0   1   0   0   0   0))
      ("K"       #(0   0   0   1   0   0   0))
      ("A"       #(0   0   0   0   1   0   0))
      ("mol"     #(0   0   0   0   0   1   0))
      ("cd"      #(0   0   0   0   0   0   1))
      ("Hz"      "s-1")
      ("N"       "kg/m.s2")
      ("Pa"      "N/m2")
      ("J"       "N.m")
      ("W"       "J/s")
      ("Cb"      "A.s")
      ("V"       "W/A")
      ("Fd"      "Cb/V")     ;; Farad
      ("Ohm"     "V/A")
      ("rad"     "m/m")
      ("sr"      "m2/m2")))  ;; steradian: solid angle

(define-unit-table *customary-units*
    ;; unit1     unit2       scale      displacement
    '(("C"       "K"           1            +273.15) ;; 1 * (x1[C] + 273.15) = x2[K]
      ("F"       "C"           5/9              -32) ;; 5/9 * (x1[F] - 32) = x2[C]
      ("bar"     "Pa"          1e5)                  ;; 1e5 * x1[bar] = x2[Pa]
      ("psi"     "Pa"          6894.76)              ;; etc...
      ("inch"    "m"           25.4e-3)
      ("ft"      "inch"        12)
      ("min"     "s"           60)
      ("h"       "s"           3600)
      ("lit"     "m3"          1e-3)
      ("L"       "m3"          1e-3)
      ("sqft"    "ft2")
      ("cfm"     "ft3/min")
      ("cal"     "J"           4.184)
      ("BTU"     "J"           1054.35)
      ("lbf"     "N"           4.448222)
      ("lbm"     "kg"          4.535924e-1)
      ("HP"      "W"           746)))   ;; electric horsepower


;;; ------------------------------------------------------------
;;; CONVERSION FUNCTIONS
;;; ------------------------------------------------------------
(defun to-si (num unit)
  (values (float (* (+ num
		       (parse-displacement unit))
		    (parse-scale unit)))
	  (si-name unit)))

(defun to-unit (num source-unit target-unit)
  (let ((source-scale (parse-scale source-unit))
	(source-displ (parse-displacement source-unit))
	(target-scale (parse-scale target-unit))
	(target-displ (parse-displacement target-unit)))
    (if (dim-equal (parse-dim source-unit) (parse-dim target-unit))
	(values (float (- (* (/ source-scale target-scale)
			     (+ num source-displ))
			  target-displ))
		target-unit)
	(error "Dimensions do not match."))))


;;; ------------------------------------------------------------
;;; READ MACRO FOR EASY CONVERSIONS: #[]
;;; ------------------------------------------------------------
(set-macro-character #\] (get-macro-character #\)))

(set-dispatch-macro-character #\# #\[
 #'(lambda (stream char1 char2)
     (declare (ignore char1 char2))
     (let ((list (read-delimited-list #\] stream t)))
       (cond ((endp (cdr list))
	      (car list))
	     ((endp (cddr list))
	      `(to-si ,(first list) ,(second list)))
	     ((endp (cdddr list))
	      `(to-unit ,(first list) ,(second list) ,(third list)))
	     (t nil)))))