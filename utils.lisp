;;; -------------------------------------------------------------
;;; FUNCTION COMPOSITION
;;; -------------------------------------------------------------
(defun compose (&rest fns)
  "Compose functions like that: (f1 (f2 (f3 x)))."
  (if fns
      (let ((fn1 (car (last fns)))
	    (fnrest (butlast fns)))
	#'(lambda (&rest args)
	    (reduce #'funcall fnrest
		    :from-end t
		    :initial-value (apply fn1 args))))
      #'identity))


(defun ident (obj)
  "Given an object, returns a functions that returns the object"
  #'(lambda (&rest args)
      (declare (ignore args))
      obj))

;;; -------------------------------------------------------------
;;; LISTS
;;; -------------------------------------------------------------
(defun flatten (x)
  "Flatten a complex list into a plain list."
  (labels ((rec (x acc)
	     (cond ((null x) acc)
		   ((atom x) (cons x acc))
		   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun mklist (obj)
  (if (listp obj) obj (list obj)))

(defun steplist (lst &key (start 0) (step 1))
  "Selects elements of a list according to a step n"
  (labels ((rec (lst step)
	     (if (null lst)
		 nil
		 (cons (car lst)
		       (rec (nthcdr step lst) step)))))
    (rec (nthcdr start lst) step)))

(defun mapcars (fn &rest lists)
  "Mapcar a function over several lists. By Paul Graham."
  (let ((result nil))
    (dolist (ilist lists)
      (dolist (obj ilist)
	(push (funcall fn obj) result)))
    (nreverse result)))

(defun enumerate (start stop &optional (step 1))
  (labels ((rec (i stop step)
	     (if (>= i stop)
		 nil
		 (cons i (rec (+ i step) stop step)))))
    (rec start stop step)))



;;; -------------------------------------------------------------
;;; SEQUENCES
;;; -------------------------------------------------------------
(defun swapelt (s i1 i2)
  "Swap two elements of a sequence."
  (let ((seq (copy-seq s)))
    (setf (elt seq i1) (elt s i2))
    (setf (elt seq i2) (elt s i1))
    seq))

(defun sum (seq)
  "Sum of the elements of a sequence."
  (reduce #'+ seq))

(defun sum-abs (seq)
  "Sum of the absolute values of the elements of a sequence."
  (reduce #'+ (map (type-of seq) #'abs seq)))

(defun seq-difference (seq1 seq2
		       &key (key nil) (start 0) (end nil) (count nil))
  "Returns the elements of SEQ1 which are not in SEQ2. The
   keyword arguments are applied only on SEQ2"
  (remove-if-not #'(lambda (i)
		     (find i seq1))
		 seq2 :start start :end end :count count :key key))

(defun nseq-difference (seq1 seq2
		       &key (key nil) (start 0) (end nil) (count nil))
  "Deletes the elements of SEQ2 which are found on SEQ1. The
   keyword arguments are applied only on SEQ2. This is the
   destructive version of SEQ-DIFFERENCE."
  (delete-if-not #'(lambda (i)
		     (find i seq1))
		 seq2 :start start :end end :count count :key key))


;;; ----------------------------------------------------------
;;; ARRAYS
;;; ----------------------------------------------------------
(defun copy-array (array)
  "Returns a copy of an array"
  (let ((arr (make-array (array-dimensions array)))
	(n (array-total-size array)))
    ;; Copy elements to new array
    (dotimes (i n arr)
      (setf (row-major-aref arr i) (row-major-aref array i)))))


;;; -------------------------------------------------------------
;;; MACROS
;;; -------------------------------------------------------------
(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
		     `(,s (gensym)))
		 syms)
     ,@body))


(defmacro abbrev (long short)
  "Define abbreviations for functions"
  `(defmacro ,short (&body body)
     `(,',long ,@body)))

;; Defined abbreviations
(abbrev multiple-value-bind mvb)