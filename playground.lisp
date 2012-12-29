
;;; - - - - - - -

;; (defun solve-q (el flv)
;;   (let* ((fl-water-in (first flv))
;;       (fl-air-in (second flv))

;;       (ta-mean (find-root #'(lambda (ta-mean)
;;                               (- (q-exchanged el
;;                                               flv
;;                                               (build-flv-mean flv ta-mean))
;;                                  (* 2
;;                                     (mfr fl-air-in)
;;                                     (cp (fluid fl-air-in) ta-mean)
;;                                     (- (tem fl-air-in)
;;                                        ta-mean))))
;;                           (tem fl-water-in)
;;                           (tem fl-air-in))))
;;     (q-exchanged el flv (build-flv-mean flv ta-mean))))







(defclass  )


(defclass)




;;; ------------------------------------------------------------
;;; LINEAR ELEMENTS
;;; ------------------------------------------------------------
(defclass linear-element (element)
  ((fn-mfr  :initform #'identity)
   (fn-tem  :initform #'identity)
   (fn-pres :initform #'identity)))

(defmethod calc-output-flows ((el linear-element) (fl flow))
  (make-instance 'flow
                 :mfr (funcall (fn-mfr el) fl)
                 :tem (funcall (fn-tem el) fl)
                 :pres (funcall (fn-pres el) fl)))


(defclass element ()
  ((passage  :initarg :passage  :reader passage)
   (in-flow  :initarg :in-flow  :reader in-flow)
   (out-flow :initarg :out-flow :reader out-flow)))


(defclass component ()
  (element-list))

(defclass compressor (component)
  ())






(defgeneric u-value (component flow)
  (:documentation "U-value (reverse of total thermal resistance"))

(defmethod u-value ((co heat-exchanger) (fl flow))
  (let ((eta1 (eta-fin (passage-1 co) (flow-1 co)))
        (eta2 (eta-fin (passage-2 co) (flow-2 co)))
        (res-1 (/ 1 eta)))))


;;; - - - - - - -

;; (defgeneric vmass (passage flow)
;;   (:documentation "Mass velocity"))

;; (defmethod vmass ((tbank tubes-bank) flow)
;;   (/ (mfr flow) (sigma tbank) (face tbank)))

(defun st-h (pas flow)
  "Stanton number for heat transfer"
  (/ (nu pas flow) (re pas flow) (pr (fluid flow))))




;; (defgeneric st-h (passage flow)
;;   (:documentation "Stanton number for heat transfer"))

;;; - - - - - - -

(defgeneric mlc-fin-factor (passage flow)
  (:documentation "m.Lc fin factor"))

(defmethod mlc-fin-factor ((pas tubes-bank-circular-fins) (fl flow))
  (let* ((t/2 (/ (fin-thick pas) 2))
         (l-fin (/ (- (fin-diam pas) (dout pas)) 2))
         (lc-fin (+ l-fin t/2))
         (ap (* lc-fin 2 t/2))
         (k (con (material pas) (tem fl)))
         (h (htc pas fl)))
    (* (expt lc-fin 3/2)
       (sqrt (/ (* 2 h)
                (*  k ap))))))


(defun eta-fin (passage flow)
  (let ((mlc (mlc-fin-factor passage flow)))
    (/ (tanh mlc)
       mlc)))


(defmethod eta-fin-2 ((pas tubes-bank-circular-fins) (fl flow))
  (let* ((h (htc pas fl))
         (k (con (material pas) (tem fl)))

         (d (fin-diam pas))
         (tau (fin-thick pas))
         (p (* 2 pi d))
         (ac (* .5 p tau))
         (l-fin (/ (- (fin-diam pas) (dout pas)) 2))

         (m (sqrt (/ (* h p) (* k ac))))
         (h/mk (/ h (* m k)))

         (ml (* m l-fin))
         (sinh-ml (sinh ml))
         (cosh-ml (cosh ml))

         (lc-fin (+ l-fin (/ tau 2)))
         (mlc (* lc-fin (sqrt (/ (* h p) (* k ac))))))



    (print ml)
    (print sinh-ml)
    (print cosh-ml)
    (print h)
    (print k)
    (values (/ (/ (+ sinh-ml (* h/mk cosh-ml))
                  (+ cosh-ml (* h/mk sinh-ml)))
               ml)
            (/ (tanh ml)
               ml)
            (/ (tanh mlc)
               mlc))))


;; (defmacro defmethod-default (name arg-list &body body)
;;   (let ((default-name (intern (concatenate 'string "DEFAULT-" (symbol-name name)))))
;;     `(progn
;;        (defun ,default-name (,(caar arg-list))
;;       ,@body)
;;        (defmethod ,default-name ,arg-list
;;       (ensure-slot ,(caar arg-list) ,name
;;         (,default-name ,(caar arg-list)))))))


;; (defmethod default-alpha (finned-tubes-bank)
;;   (let* ((st (tube-pitch ftb))
;;       (sl (row-pitch ftb))
;;       (d (dout ftb))
;;       (fp (fin-pitch ftb))
;;       (ft (fin-thick ftb)))
;;     (let ((a-base (* pi d (- 1 (/ ft fp))))
;;        (a-fin (* 2 ;; the fin has two sides!
;;                  (/ 1 fp)
;;                  (- (* st sl)
;;                     (* 0.25 pi (sq d))))))
;;       (/ (+ a-base a-fin)
;;       (* st sl)))))


;; (defmethod default-fin-area/total-area ((ftb finned-tubes-bank))
;;   (let* ((st (tube-pitch ftb))
;;       (sl (row-pitch ftb))
;;       (d (dout ftb))
;;       (fp (fin-pitch ftb))
;;       (ft (fin-thick ftb)))
;;     (let ((a-base (* pi d (- 1 (/ ft fp))))
;;        (a-fin (* 2 ;; the fin has two sides!
;;                  (/ 1 fp)
;;                  (- (* st sl)
;;                     (* 0.25 pi (sq d))))))
;;       (/ a-fin (+ a-base a-fin)))))

(defgeneric normalized-fin-area (finned-tubes-bank))

(defmethod normalized-fin-area ((ftb finned-tubes-bank))
  (let ((st (tube-pitch ftb))
        (sl (row-pitch ftb))
        (d (dout ftb))
        (fp (fin-pitch ftb)))
    (* 2 ;; the fin has two sides!
       (/ 1 fp)
       (- (* st sl)
          (* 0.25 pi (sq d))))))


(defmethod default-fin-area/total-area ((ftb finned-tubes-bank))
  (let* ((st (tube-pitch ftb))
         (sl (row-pitch ftb))
         (d (dout ftb))
         (fp (fin-pitch ftb))
         (ft (fin-thick ftb)))
    (let ((a-base (* pi d (- 1 (/ ft fp))))
          (a-fin (* 2 ;; the fin has two sides!
                    (/ 1 fp)
                    (- (* st sl)
                       (* 0.25 pi (sq d))))))
      (values
       ;; sigma
       (/ (if (< sd (/ (+ st d) 2))
              (* 2 (- sd d (* ft sd (/ 1 fp))))
              (- st d (* ft st (/ 1 fp))))
          st)
       ;; fin-area/total-area
       (/ a-fin (+ a-base a-fin))
       ;; heat-exchange-area/total-volume
       (/ )))))



;;; this fails because we have :arg-fn and :val-fn
;;; for table-based properties
(defun make-property (name data)
  (typecase data
    (number (make-instance 'constant-property
                           :name name
                           :value data))
    (function (make-instance 'function-property
                             :name name
                             :function data))
    (array (make-instance 'table-property
                          :name name
                          :table data))))


;;; --- Functions for flows ---
(defgeneric vel (flow)
  (:documentation "Velocity"))

(defmethod vel ((fl internal-flow))
  (/ (mfr fl) (area (face (conduit fl)))))


(defgeneric re (flow)
  (:documentation "Reynolds number"))

(defmethod re ((fl internal-flow))
  (/ (* (rho (fluid flow))
        (vel flow)
        (dhyd (face (conduit flow))))
     (vis (fluid flow))))


(defgeneric nu (flow)
  (:documentation "Nusselt number"))

(defmethod nu ((fl internal-flow))
  (gnielinski (pr fl)
              (re fl)
              (ff fl)))


(defgeneric ff (conduit flow)
  (:documentation "Friction factor"))

(defmethod ff ((dc duct) (fl turbulent-flow))
  (sq (find-root (colebrook (diam (duct fl))
                            (re fl)
                            (rough (duct fl)))
                 1d-8 1d+8)))


(defclass flow ()
  ((fluid :documentation "Fluid of the flow"
          :accessor fluid)
   (mfr :documentation "Mass flow rate [kg/s]"
        :initarg :mfr :accessor mfr)
   (tem :documentation "Temperature [K]"
        :initarg :tem :accessor tem)
   (pres :documentation "Pressure [Pa]"
         :initarg :pres :accessor pres)
   (lam-nu :initarg :lam-nu :accessor lam-nu)
   (turb-nu :initarg :turb-nu :accessor turb-nu)
   (lam-ff :initarg :lam-ff :accessor lam-ff)
   (turb-ff :initarg :turb-ff :accessor turb-ff)))

;;; ------------------------------------------------------------
;;; PROPERTIES
;;; ------------------------------------------------------------
(defclass property ()
  ((name :initarg :name :reader name)))

(defclass table-property (property)
  ((table :initarg :table :reader tab)
   (arg-fn :documentation "Functions applied to the arguments before table lookup"
           :initarg :arg-fn :initform #'identity :reader arg-fn)
   (val-fn :documentation "Function applied to the looked-up table value"
           :initarg :val-fn :initform #'identity :reader val-fn)))

(defclass function-property (property)
  ((function :initarg :function :reader func)))

(defclass constant-property (property)
  ((value :initarg :value :reader val)))


(defgeneric eval-property (prop &rest args)
  (:documentation "Evaluate a property for some arguments."))

(defmethod eval-property ((prop table-property) &rest args)
  (funcall (val-fn prop)
           (apply #'lookup-at-table (tab prop) (mapcar (arg-fn prop) args))))

(defmethod eval-property ((prop function-property) &rest args)
  (apply (func prop) args))

(defmethod eval-property ((prop constant-property) &rest args)
  (declare (ignore args))
  (val prop))


(defparameter *aircon* #2a((-183 0.0083)
                           (-173 0.0092)
                           (-163 0.0102)
                           (-153 0.0111)
                           (-143 0.0120)
                           (-140 0.0123)
                           (-133 0.0129)
                           (-123 0.0138)
                           (-113 0.0146)
                           (-93  0.0164)
                           (-73  0.0181)
                           (-53  0.0198)
                           (-33  0.0215)
                           (-13  0.0231)
                           (7    0.0246)
                           (27   0.0261)
                           (47   0.0276)
                           (67   0.0290)
                           (87   0.0304)
                           (107  0.0317)
                           (127  0.0331)
                           (147  0.0344)
                           (167  0.0357)
                           (187  0.0370)
                           (207  0.0383)
                           (227  0.0395)
                           (327  0.0456)))















;;; ------------------------------------------------------------
;;; TABLE FUNCTIONS
;;; ------------------------------------------------------------
(defun lambda-table (table &key (arg-fn #'identity) (val-fn #'identity))
  #'(lambda (arg)
      (funcall val-fn
               (funcall #'lookup-at-table table (funcall arg-fn arg)))))

(defmacro defun-table (name table &key (arg-fn #'identity) (val-fn #'identity))
  `(defun ,name (arg)
     (funcall ,val-fn
              (funcall #'lookup-at-table ,table (funcall ,arg-fn arg)))))

;;; ------------------------------------------------------------
;;; FLUID DEFINITIONS
;;; ------------------------------------------------------------
(defparameter *air*
  (make-instance 'fluid
                 :molecular-weight 29d-3
                 :density #'(lambda (tem p)
                              (/ (* p (mw *air*))
                                 (* +rgas+ tem)))
                 :conductivity (lambda-table #2a((-183 0.0083)
                                                 (-173 0.0092)
                                                 (-163 0.0102)
                                                 (-153 0.0111)
                                                 (-143 0.0120)
                                                 (-140 0.0123)
                                                 (-133 0.0129)
                                                 (-123 0.0138)
                                                 (-113 0.0146)
                                                 (-93  0.0164)
                                                 (-73  0.0181)
                                                 (-53  0.0198)
                                                 (-33  0.0215)
                                                 (-13  0.0231)
                                                 (7    0.0246)
                                                 (27   0.0261)
                                                 (47   0.0276)
                                                 (67   0.0290)
                                                 (87   0.0304)
                                                 (107  0.0317)
                                                 (127  0.0331)
                                                 (147  0.0344)
                                                 (167  0.0357)
                                                 (187  0.0370)
                                                 (207  0.0383)
                                                 (227  0.0395)
                                                 (327  0.0456))
                                             :arg-fn #'celcius)
                 :viscosity (lambda-table #2a((-183   0.00635)
                                              (-173   0.00706)
                                              (-163   0.00775)
                                              (-153   0.00843)
                                              (-143   0.00909)
                                              (-140   0.00929)
                                              (-133   0.00974)
                                              (-123   0.01038)
                                              (-113   0.0110)
                                              (-93    0.0122)
                                              (-73    0.0134)
                                              (-53    0.0145)
                                              (-33    0.0155)
                                              (-13    0.0166)
                                              (7      0.0176)
                                              (27     0.0185)
                                              (47     0.0195)
                                              (67     0.0204)
                                              (87     0.0213)
                                              (107    0.0221)
                                              (127    0.0229)
                                              (147    0.0238)
                                              (167    0.0245)
                                              (187    0.0253)
                                              (207    0.0261)
                                              (227    0.0268)
                                              (327    0.0303))
                                          :arg-fn #'celcius
                                          :val-fn #'(lambda (x) (* x 1e-3)))
                 :specific-heat (lambda-table #2a((-173 1.028)
                                                  (-163 1.022)
                                                  (-153 1.017)
                                                  (-143 1.014)
                                                  (-140 1.013)
                                                  (-133 1.012)
                                                  (-123 1.011)
                                                  (-113 1.009)
                                                  (-93  1.007)
                                                  (-73  1.006)
                                                  (-53  1.006)
                                                  (-33  1.005)
                                                  (-13  1.005)
                                                  (7    1.006)
                                                  (27   1.006)
                                                  (47   1.007)
                                                  (67   1.008)
                                                  (87   1.010)
                                                  (107  1.012)
                                                  (127  1.014)
                                                  (147  1.017)
                                                  (167  1.020)
                                                  (187  1.023)
                                                  (207  1.026)
                                                  (227  1.030)
                                                  (327  1.052))
                                              :arg-fn #'celcius)))


;;; ------------------------------------------------------------
;;; MATTER
;;; ------------------------------------------------------------
(defclass matter ()
  ((molecular-weight :initarg :molecular-weight :reader molecular-weight)
   (density :initarg :density :reader density)
   (conductivity :initarg :conductivity :reader conductivity)
   (specific-heat :initarg :specific-heat :reader specific-heat)))

(defclass solid (matter)
  ())

(defclass fluid (matter)
  ((viscosity :initarg :viscosity :reader viscosity)))


;;; --- Functions for properties of matter ---
(defun mw (matter)
  (molecular-weight matter))

(defun rho (matter tem &optional (pres +patm+))
  "Density"
  (funcall (density matter) tem pres))

(defun con (matter tem)
  "Conductivity"
  (funcall (conductivity matter) tem))

(defun cp (matter tem)
  "Heat capacity at constant pressure"
  (funcall (specific-heat matter) tem))

(defun thermal-diff (matter tem &optional (pres +patm+))
  "Thermal diffusivity"
  (/ (con matter tem)
     (* (rho matter tem pres)
        (cp matter tem))))

(defun vis (fluid tem)
  "Viscosity"
  (funcall (viscosity matter) tem))

(defun kvis (fluid tem &optional (pres +patm+))
  "Kinematic viscosity"
  (/ (vis matter tem) (rho matter tem pres)))







































(defmethod vmean (conduit flow))


(defmethod re (conduit flow))


(defmethod nu (conduit flow))

(defmethod nu (finned-tubes-bank flow))

(defmethod nu )

(defmethod ff (pipe flow))

(defmethod ff (duct flow))








(defclass component ()
  ())

(defclass compressor (component)
  ())

(defclass hex (component)
  (passage-1)
  (passage-2))

(defclass finned-tube-hex (hex))




(defun jh (re)
  (funcall (lambda-line 4000 0.015 70000 0.005) re))

(defun jh (re)
  (funcall (lambda-line 5000 0.008 7000 0.007) re))


(defun lambda-line (x1 y1 x2 y2)
  #'(lambda (x)
      (interpolate x1 y1 x2 y2 x)))


(defmacro defun-table (name table &key (arg-fn #'identity) (val-fn #'identity))
  `(defun ,name (arg)
     (funcall ,val-fn
              (lookup-at-table ,table (funcall ,arg-fn arg)))))
