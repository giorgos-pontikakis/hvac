;;; ------------------------------------------------------------
;;; TABLES
;;; ------------------------------------------------------------
(defun make-table (assoc-list &key (test 'eql))
  "Make a table from an assoc list"
  (let ((ht (make-hash-table :test test)))
    (if (listp assoc-list)
	(mapcar #'(lambda (pair)
		    (let ((k (car pair))
			  (e (cdr pair)))
		      (setf (gethash k ht) e)))
		assoc-list)
	nil)
    ht))


(defun entry (table &optional (key nil key-p))
  (cond
    ;; if key is provided, return a list of all entries
    ((not key-p) (let ((entry-list nil))
		   (maphash #'(lambda (k e)
				(declare (ignore k))
				(push e entry-list))
			    table)
		   entry-list))
    ;; otherwise, return entry normally
    (t (gethash key table))))


(defun key (table &optional (entry nil entry-p) (test #'eql))
  (cond
    ;; if entry is provided, return a list of all keys
    ((not entry-p) (let ((key-list nil))
		     (maphash #'(lambda (k e)
				  (declare (ignore e))
				  (push k key-list))
			      table)
		     key-list))
    ;; otherwise, return all matching keys
    (t (let ((key-list nil))
	 (maphash #'(lambda (k e)
		      (when (funcall test e entry)
			(push k key-list)))
		  table)
	 key-list))))


(defun key-exists-p (table key)
  (nth-value 1 (gethash key table)))


(defun (setf entry) (new-val table key)
  (setf (gethash key table) new-val)
  new-val)


