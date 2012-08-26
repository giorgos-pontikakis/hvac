

;;; MACROS

(defmacro provide-slot-default (obj slot &body body)
  "If the slot of object is unbound, set it to a default value
   (which is calculated within the body). This "
  `(unless (slot-boundp ,obj ,slot)
     (setf (slot-value ,obj ,slot)
           ,@body)))



;;; PASSAGES

(defclass passage ()
  ((face      :initarg :face     :reader face)
   (material  :initarg :material :reader material)
   (tem       :initarg :tem      :accessor tem)))




;;; BODIES (for external flows)

(defclass body (passage)
  ())

(defclass tubes-bank (body)
  ((dout       :initarg :dout       :reader dout)
   (row-pitch  :initarg :row-pitch  :reader row-pitch)
   (tube-pitch :initarg :tube-pitch :reader tube-pitch)
   (dhyd       :initarg :dhyd)    ;; reader defined with defmethod below
   (sigma      :initarg :sigma      :reader sigma )
   (alpha      :initarg :alpha      :reader alpha)))

(defclass fins-mixin ()
  ((fin-thick  :initarg :fin-thick  :reader fin-thick)
   (fin-pitch  :initarg :fin-pitch  :reader fin-pitch)
   (af/at      :initarg :af/at      :reader af/at)))

(defclass tubes-bank-continuous-fins (tubes-bank fins-mixin)
  ())

(defclass tubes-bank-circular-fins (tubes-bank fins-mixin)
  ((fin-diam :initarg :fin-diam :reader fin-diam)))

(defclass kays-cf-7.0-5/8j (tubes-bank-circular-fins)
  ()
  (:default-initargs :dout       (to-si 0.645 "inch")
                     :row-pitch  (to-si 1.35 "inch")
                     :tube-pitch (to-si 1.232 "inch")
                     :dhyd       (to-si 0.0219 "ft")
                     :sigma      0.449
                     :alpha      269
                     :fin-thick  (to-si 0.010 "inch")
                     :fin-pitch  1/276
                     :af/at      0.830
                     :fin-diam   (to-si 1.121 "inch")))

(defclass kays-8.0-3/8t (tubes-bank-continuous-fins)
  ()
  (:default-initargs :dout       (to-si 0.402 "inch")
                     :row-pitch  (to-si 0.866 "inch")
                     :tube-pitch (to-si 1.0 "inch")
                     :dhyd       (to-si 0.01192 "ft")
                     :sigma      0.534
                     :alpha      587
                     :fin-thick  (to-si 0.013 "inch")
                     :fin-pitch  1/315
                     :af/at      0.913))

(defclass kays-7.75-5/8t (tubes-bank-continuous-fins)
  ()
  (:default-initargs :dout       (to-si 0.676 "inch")
                     :row-pitch  (to-si 1.75 "inch")
                     :tube-pitch (to-si 1.50 "inch")
                     :dhyd       (to-si 0.01192 "ft")
                     :sigma      0.481
                     :alpha      554
                     :fin-thick  (to-si 0.016 "inch")
                     :fin-pitch  1/305
                     :af/at      0.950))

(defmethod initialize-instance :after ((obj tubes-bank) &key)
  (provide-slot-default obj 'dhyd
                        (default-dhyd obj))
  (provide-slot-default obj 'sigma
                        (default-sigma obj))
  (provide-slot-default obj 'alpha
                        (default-alpha obj)))

(defmethod initialize-instance :after ((obj fins-mixin) &key)
  (provide-slot-default obj 'af/at
                        (default-af/at obj)))




;;; CONDUITS (for internal flows)

(defclass conduit (passage)
  ((rough :initarg :rough :reader rough)))

(defclass duct (conduit)
  ())

(defclass tube (conduit)
  ())



;;; FACES (shapes for passage cross-sections)

(defclass face ()
  ((name :initarg :name :reader name)))

(defclass circle (face)
  ((diam :initarg :diam :reader diam)))

(defclass rectangle (face)
  ((width  :initarg :width  :reader width)
   (height :initarg :height :reader height)))



;;; METHODS FOR PASSAGES AND THEIR FACES

;;; Hydraulic diameter

(defgeneric dhyd (passage-or-face)
  (:documentation "Hydraulic diameter"))

(defmethod dhyd ((tb tubes-bank))
  (slot-value tb 'dhyd))

(defmethod dhyd ((cn conduit))
  (dhyd (face cn)))

(defmethod dhyd ((c circle))
  (diam c))

(defmethod dhyd ((r rectangle))
  (/ (* 4 (area r))
     (circum r)))

;;; - - - - - - -

(defgeneric area (face)
  (:documentation "Area"))

(defmethod area ((c circle))
  (* 0.25 pi (expt (diam c) 2)))

(defmethod area ((r rectangle))
  (* (width r)
     (height r)))

;;; - - - - - - -

(defgeneric circum (face)
  (:documentation "Circumference"))

(defmethod circum ((c circle))
  (* pi (diam c)))

(defmethod circum ((r rectangle))
  (* 2 (+ (width r)
          (height r))))

;;; - - - - - - -

(defgeneric default-dhyd (tubes-bank))

(defmethod default-dhyd ((tb tubes-bank))
  (* 4
     (row-pitch tb)
     (/ (normalized-free-flow-area tb)
        (+ (normalized-base-area tb)
           (normalized-fin-area tb)))))

;;; - - - - - - -

(defgeneric default-sigma (tubes-bank)
  (:documentation "Calculated flow area/frontal area"))

(defmethod default-sigma ((tbank tubes-bank))
  (/ (normalized-free-flow-area tbank)
     (normalized-frontal-area tbank)))

;;; - - - - - - -

(defgeneric default-alpha (tubes-bank)
  (:documentation "Calculated heat transfer area/total volume"))

(defmethod default-alpha ((tb tubes-bank))
  (let ((a-base (normalized-base-area tb))
        (a-fin (normalized-fin-area tb))
        (vol (normalized-volume tb)))
    (/ (+ a-base a-fin)
       vol)))

;;; - - - - - - -

(defgeneric default-af/at (tubes-bank-continuous-fins)
  (:documentation "Ratio of finned area to total area"))

(defmethod default-af/at ((tb tubes-bank-continuous-fins))
  (let ((a-base (normalized-base-area tb))
        (a-fin (normalized-fin-area tb)))
    (/ a-fin (+ a-base a-fin))))

;;; - - - - - - -

(defgeneric normalized-free-flow-area (tubes-bank)
  (:documentation "Free flow area per unit length of one tube"))

(defmethod normalized-free-flow-area ((tbank tubes-bank))
  (let* ((st (tube-pitch tbank))
         (sl (row-pitch tbank))
         (d (dout tbank))
         (sd (sqrt (+ (sq sl) (sq (* 0.5 st))))))
    (if (< sd (/ (+ st d) 2))
        (* 2 (- sd d))
        (- st d))))

(defmethod normalized-free-flow-area ((tb tubes-bank-continuous-fins))
  (let* ((st (tube-pitch tb))
         (sl (row-pitch tb))
         (d (dout tb))
         (ft (fin-thick tb))
         (fp (fin-pitch tb))
         (sd (sqrt (+ (sq sl) (sq (* 0.5 st))))))
    (if (< sd (/ (+ st d) 2))
        (* 2 (- sd d (* ft sd (/ 1 fp))))
        (- st d (* ft st (/ 1 fp))))))

;;; - - - - - - -

(defgeneric normalized-frontal-area (tubes-bank)
  (:documentation "Frontal area per unit length of one tube"))

(defmethod normalized-frontal-area ((tbank tubes-bank))
  (tube-pitch tbank))

;;; - - - - - - -

(defgeneric normalized-fin-area (tubes-bank)
  (:documentation "Fin area per unit length of one tube"))

(defmethod normalized-fin-area ((tb tubes-bank))
  ;; no fins
  0.0)

(defmethod normalized-fin-area ((tb tubes-bank-continuous-fins))
  (let ((st (tube-pitch tb))
        (sl (row-pitch tb))
        (d (dout tb))
        (fp (fin-pitch tb)))
    (* 2 ;; the fin has two sides!
       (/ 1 fp)
       (- (* st sl)
          (* 0.25 pi (sq d))))))

(defmethod normalized-fin-area ((tb tubes-bank-circular-fins))
  (let ((dtube (dout tb))
        (dfin (fin-diam tb))
        (fp (fin-pitch tb)))
    (* 2 ;; the fin has two sides!
       (/ 1 fp)
       (- (* 0.25 pi (sq dfin))
          (* 0.25 pi (sq dtube))))))

;;; - - - - - - -

(defgeneric normalized-base-area (tubes-bank)
  (:documentation "Base area per unit length of one tube"))

(defmethod normalized-base-area ((tb tubes-bank))
  (let ((d (dout tb)))
    (* pi d)))

(defmethod normalized-base-area ((tb tubes-bank-continuous-fins))
  (let ((d (dout tb))
        (ft (fin-thick tb))
        (fp (fin-pitch tb)))
    (* pi d (- 1 (/ ft fp)))))

;;; - - - - - - -

(defgeneric normalized-volume (tubes-bank)
  (:documentation "Heat exchanger volume per unit length of one tube"))

(defmethod normalized-volume ((tb tubes-bank))
  (let ((st (tube-pitch tb))
        (sl (row-pitch tb)))
    (* st sl)))

;;; - - - - - - -
