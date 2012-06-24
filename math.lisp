;;; ------------------------------------------------------------
;;; ROOT FINDING
;;; ------------------------------------------------------------
(defun bisection (func x-neg x-pos x-acc)
  (let* ((dx (abs (- x-pos x-neg)))
	 (x-mid (* 0.5d0 (+ x-neg x-pos)))
	 (f-mid (funcall func x-mid)))
    (cond ((or (< dx x-acc) (= f-mid 0.d0))
	   (values x-mid f-mid))
	  ((< f-mid 0.d0)
	   (bisection func x-mid x-pos x-acc))
	  ((> f-mid 0.d0)
	   (bisection func x-neg x-mid x-acc)))))


(defun find-root (func x1 x2 &optional (xacc 1d-6))
  "Find a root of a function in an interval [x1 x2] using the
   bisection method. Returns NIL if the limits x1 & x2 do not
   bracket a root."
  (let ((f1 (funcall func x1))
	(f2 (funcall func x2)))
    (cond ((< 0.d0 (* f1 f2)) nil)
	  ((< f1 0.d0) (bisection func x1 x2 xacc))
	  (t (bisection func x2 x1 xacc)))))


;;; ------------------------------------------------------------
;;; SMALL BUT USEFUL
;;; ------------------------------------------------------------
(defun sign (a)
  "Return -1 if a<0, +1 otherwise"
  (if (< a 0)
      -1
      +1))

(defun sq (x)
  "Square of x"
  (expt x 2))

(defun cb (x)
  "Cube of x"
  (expt x 3))

(defun avg (&rest args)
  "Average of the arguments."
  (/ (reduce #'+ args)
     (length args)))

(defun linear-interp (x1 y1 x2 y2 x)
  "Interpolate linearly between two points"
  (+ y1
     (* (/ (- y2 y1)
	   (- x2 x1))
	(- x x1))))

(defun deriv (fn x &key (eps 1d-3))
  "Derivative of a function fn at a point x"
  (/ (- (funcall fn (+ x eps))
	(funcall fn (- x eps)))
     (* 2 eps)))


;;; ----------------------------------------------------------
;;; UTILITIES FOR MATRICES (2D ARRAYS)
;;; ----------------------------------------------------------
(defun array-num-rows (arr-in)
  "Return the number of rows of a 2D array"
  (array-dimension arr-in 0))

(defun array-num-cols (arr-in)
  "Return the number of columns of a 2D array"
  (array-dimension arr-in 1))

(defun arow (arr-in i)
  "Returns a vector which is a part of a 2D array"
  (let* ((ncols (array-dimension arr-in 1))
	 (result (make-array ncols)))
    (dotimes (j ncols result)
      (setf (svref result j) (aref arr-in i j)))))

(defun acol (arr-in j)
  "Returns a vector which is a column of a 2D array"
  (let* ((nrows (array-dimension arr-in 0))
	 (result (make-array nrows)))
    (dotimes (i nrows result)
      (setf (svref result i) (aref arr-in i j)))))

(defun swaprows (arr-in i1 i2)
  "Swaps two rows of a 2D array"
  (let ((arr (copy-array arr-in))
	(ncols (array-dimension arr-in 1)))
    (dotimes (j ncols arr)
      (setf (aref arr i1 j) (aref arr-in i2 j))
      (setf (aref arr i2 j) (aref arr-in i1 j)))))

(defun swapcols (arr-in j1 j2)
  "Swaps two columns of a 2D array"
  (let ((arr (copy-array arr-in))
	(nrows (array-dimension arr-in 0)))
    (dotimes (i nrows arr)
      (setf (aref arr i j1) (aref arr-in i j2))
      (setf (aref arr i j2) (aref arr-in i j1)))))


;;; --- Fundamental operations ---
(defun mat+ (arr1 arr2)
  "Add two arrays"
  (let ((n (array-total-size arr1))
	(arr (make-array (array-dimensions arr1))))
    (dotimes (i n arr)
      (setf (row-major-aref arr i)
	    (+ (row-major-aref arr1 i)
	       (row-major-aref arr2 i))))))

(defun mat- (arr1 arr2)
  "Subtract two arrays"
  (let ((n (array-total-size arr1))
	(arr (make-array (array-dimensions arr1))))
    (dotimes (i n arr)
      (setf (row-major-aref arr i)
	    (- (row-major-aref arr1 i)
	       (row-major-aref arr2 i))))))


(defgeneric mat* (a b)
  (:documentation
   "Multiplies two arrays or an array with a scalar."))

(defmethod mat* ((a array) (b array))
  (let* ((rows (array-dimension a 0))
	 (cols (array-dimension b 1))
	 (c (make-array (list rows cols))))
    (dotimes (i rows c)
      (dotimes (j cols)
	(setf (aref c i j)
	      (sum (map 'vector #'*
			(arow a i)
			(acol b j))))))))

(defmethod mat* ((a number) (b array))
  (let ((n (array-total-size b))
	(c (make-array (array-dimensions b))))
    (dotimes (i n c)
      (setf (row-major-aref c i)
	    (* a (row-major-aref b i))))))



;;; ------------------------------------------------------------
;;; LOOK-UP A VALUE AT A 2D ARRAY
;;; ------------------------------------------------------------
(defun lookup-at-table (arr2d val &key (x-col 0) (y-col 1))
  (multiple-value-bind (i1 i2 x1 x2)
      (bracket-value-at-table arr2d val x-col)
    (linear-interp x1 (aref arr2d i1 y-col)
		   x2 (aref arr2d i2 y-col) val)))

(defun bracket-value-at-table (arr x-target &optional (x-col 0))
  (let* ((i-min 0)
	 (i-max (1- (array-dimension arr 0)))
	 (x-min (aref arr 0 x-col))
	 (x-max (aref arr i-max x-col)))
    (if (or (> x-min x-target) (< x-max x-target))
	(progn
	  (warn "Target value ~A not in table -- returning nearest extremum." x-target)
	  (if (< x-min x-target)
	      (values i-min (1+ i-min) x-min (aref arr (1+ i-min) x-col))
	      (values (1- i-max) i-max (aref arr (1- i-max) x-col) i-max)))
	(discrete-bisection arr x-col x-target i-min i-max))))

(defun discrete-bisection (arr x-col x-target i-min i-max)
  (let* ((i (floor (* 0.5 (+ i-max i-min))))
	 (x-i (aref arr i x-col))
	 (x-i-next (aref arr (1+ i) x-col)))
    (cond ((or (and (< x-i x-target)
		    (> x-i-next x-target))
	       (= x-i x-target)
	       (= x-i-next x-target))
	   (values i (1+ i) x-i x-i-next))
	  ((and (< x-i x-target)
		(< x-i-next x-target))
	   (discrete-bisection arr x-col x-target (1+ i) i-max))
	  ((> x-i-next x-target)
	   (discrete-bisection arr x-col x-target i-min i)))))


;;; ------------------------------------------------------------
;;; ANONYMOUS FUNCTIONS FROM POINTS OF GRAPHS
;;; ------------------------------------------------------------
(defun lambda-table (table &key (arg-fn #'identity) (val-fn #'identity))
  #'(lambda (x)
      (funcall val-fn
	       (lookup-at-table table (funcall arg-fn x)))))

(defun lambda-line (x1 y1 x2 y2)
  #'(lambda (x)
      (linear-interp x1 y1 x2 y2 x)))
