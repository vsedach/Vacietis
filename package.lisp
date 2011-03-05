(in-package #:cl)

(defpackage #:vacietis
  (:use #:cl #:named-readtables)
  (:export
   #:vacietis
   #:c-readtable))

(in-package #:vacietis)

(defreadtable vacietis
  (:merge :standard)
  (:case :invert))

(defpackage #:vacietis.reader
  (:use #:cl #:vacietis #:named-readtables))
