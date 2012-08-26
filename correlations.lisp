(in-package :hvac)



;;; NAMED CORRELATIONS
;;; Empirical correlations for the characteristics of flow

(defun colebrook-fn (diam re e-rough)
  "Returns the colebrook equation transformed as a function of
   the root of the friction factor f."
  #'(lambda (rootff)
      (- (/ 1 rootff)
         (* -2
            (log (+ (/ 2.51 re rootff)
                    (/ e-rough 3.7 diam))
                 10d0)))))

(defun ff-haaland (diam re e-rough)
  "Haaland empirical equation for the calculation of the friction
   factor for rough tubes."
  ;; multiplication by 4 converts from friction coefficient C_f
  ;; (Fanning friction factor) to friction factor f
  (* 4
     (sq (/ 1
            (* -3.6 (log (+ (/ 6.9 re)
                            (expt (/ e-rough diam 3.7) 10/9))
                         10))))))

(defun nu-gnielinski (re pr ff)
  "Gnielinski empirical correlation for the calculation of the
   Nusselt number, for fully developed (hydrodynamically and thermally)
   turbulent flow in circular tubes."
  (let ((ff/8 (/ ff 8)))
    (/ (* ff/8
          (- re 1000)
          pr)
       (+ 1
          (* 12.7
             (sqrt ff/8)
             (- (expt pr 2/3) 1))))))

(defun nu-dittus-boelter (re pr)
  "Dittus-Boelter equation for the calculation of the Nusselt
   number, for fully developed (hydrodynamically and thermally)
   turbulent flow in smooth circular tubes. The Prandtl
   coefficient used here is for cooling."
  ;; Fairly inaccurate -- for testing purposes only!
  (* 0.023
     (expt re 0.8)
     (expt pr 0.3)))
