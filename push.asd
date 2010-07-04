;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem push
  :description "Some game with input"
  :depends-on (glaw glaw-imago glop)
  :serial t
  :components
  ((:file "package")
   (:file "main")))

