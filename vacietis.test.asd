;;;; -*- lisp -*-

(defsystem :vacietis.test
  ((:module :test
            :serial t
            :components ((:file "package"))))
  :depends-on (:vacietis))
