(in-package #:vacietis.libc.string.h)
(in-readtable vacietis)

(defun/1 strerror (errnum)
  (aref vacietis.libc.errno.h::errno-strings errnum))

(load-libc-file "string.c" #.(libc-dir))
