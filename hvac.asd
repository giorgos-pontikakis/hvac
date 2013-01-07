;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl)

(asdf:defsystem :hvac
  :serial t
  :depends-on (:alexandria
               :lisputils)
  :components ((:file "package")
               (:file "constants")
               (:file "math")
               (:file "correlations")))
