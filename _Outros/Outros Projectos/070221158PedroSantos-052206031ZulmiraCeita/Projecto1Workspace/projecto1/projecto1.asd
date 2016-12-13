;;;; 2009-12-10 21:06:03
;;;;
;;;; Think of this as your project file.
;;;; Keep it up to date, and you can reload your project easily
;;;;  by right-clicking on it and selecting "Load Project"

(defpackage #:projecto1-asd
  (:use :cl :asdf))

(in-package :projecto1-asd)

(defsystem projecto1
  :name "projecto1"
  :version "0.1"
  :serial t
  :components ((:file "defpackage")
               (:file "Projecto" :depends-on ("defpackage"))
               (:file "puzzle" :depends-on ("defpackage"))
               (:file "Procura" :depends-on ("defpackage"))
               ; As you add files to your project,
               ; make sure to add them here as well
               
               )
  :depends-on ())
