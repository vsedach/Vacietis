(in-package #:cl)

(defpackage #:vacietis.test
  (:use #:cl #:named-readtables #:eos #:vacietis)
  (:intern #:eval-test
           #:reader-test)
  (:export #:run-tests))

(defpackage #:vacietis.test.reader
  (:use #:vacietis.c)
  (:import-from #:vacietis.test #:reader-test))

(defpackage #:vacietis.test.basic
  (:use #:cl #:named-readtables #:eos)
  (:import-from #:vacietis.test #:eval-test))
