(in-package :cl-user)


;;; ------------------------------------------------------------
;;; Load third party libraries
;;; SBCL
#+sbcl (require :cl-fad)
#+sbcl (require :split-sequence)
#+sbcl (require :iterate)

;;; CMUCL
;; #+cmu (asdf:operate 'asdf:load-op :cl-fad)
;; #+cmu (asdf:operate 'asdf:load-op :split-sequence)
;; #+cmu (asdf:operate 'asdf:load-op :iterate)

;;; CLISP
#+clisp (asdf:operate 'asdf:load-op :cl-fad)
#+clisp (asdf:operate 'asdf:load-op :split-sequence)
#+clisp (asdf:operate 'asdf:load-op :iterate)


;;; ------------------------------------------------------------
;;; Global optimization settings
(proclaim '(optimize
	    (speed 0)
	    (space 0)
	    (safety 3)
	    (debug 3)))


(use-package :split-sequence)
(use-package :cl-fad)
(use-package :iterate)


(defun compile-and-load (path files)
  (let ((compiled-suffix #+clisp "fas"
			 #+sbcl "fasl"
			 #+cmu "x86f"))
    (with-compilation-unit ()
      (dolist (n files)
	(let ((source-path (make-pathname :name n :type "lisp"
					  :version nil
					  :defaults path))
	      (compiled-path (make-pathname :name n :type compiled-suffix
					    :version nil
					    :defaults path)))
	  (compile-file source-path)
	  (load compiled-path))))))


;;; ------------------------------------------------------------
;;; Load files from the current directory
(let* ((base-path (make-pathname
		   :name nil :type nil :version nil
		   :defaults (parse-namestring *load-truename*)))
       (lib-path (merge-pathnames
		  (make-pathname
		   :name nil :type nil :version nil
		   :directory '(:relative "lisplib"))
		  base-path))

       (lib-files '())

       (base-files '("utils"
		     "tables"
		     "math"
		     "units"
		     "constants"
		     "correlations"
		     "matter"
		     "passages"
		     "flows"
		     "elements"
		     "hex"
		     "ducts"
		     "apps")))

  (compile-and-load lib-path lib-files)
  (compile-and-load base-path base-files))
