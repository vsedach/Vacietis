;;;; -*- lisp -*-

(defsystem :vacietis
  :name "vacietis"
  :author "Vladimir Sedach <vsedach@gmail.com>"
  :description "C to Common Lisp compiler"
  :licence "LLGPLv3"
  :serial t
  :components
  ((:static-file "vacietis.asd")
   (:static-file "vacietis.test.asd")
   (:file "package")
   (:file "implementation")
   (:file "reader"))
  :depends-on (:named-readtables :anaphora :babel :cl-ppcre))
