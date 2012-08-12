;;;; -*- lisp -*-

(defsystem :vacietis.test
  :author "Vladimir Sedach <vsedach@gmail.com"
  :license "Public Domain"
  :components
  ((:module :test
            :serial t
            :components ((:file "package")
                         (:file "test")
                         (:file "reader-tests")
                         (:file "basic-tests")
                         (:file "program-tests"))))
  :depends-on (:vacietis :eos))
