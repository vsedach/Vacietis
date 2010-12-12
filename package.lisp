(in-package #:cl)

(defpackage #:vacietis
  (:use #:cl #:named-readtables)
  (:export
   #:vacietis))

(in-package #:vacietis)

(defreadtable vacietis
  (:merge :standard)
  (:case :invert))

(defpackage #:reader.vacietis
  (:use #:cl #:vacietis #:named-readtables))
