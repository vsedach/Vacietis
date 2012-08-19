(in-package #:vacietis.libc.string.h)
(in-readtable vacietis)

(defun strerror (errnum)
  (aref vacietis.libc.errno.h::errno-strings errnum))
