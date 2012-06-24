;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl)

(asdf:defsystem :heat
  :serial t
  :depends-on (:iterate
               :alexandria
               :lisputils)
  :components ((:file "package")
               (:file "constants")))
