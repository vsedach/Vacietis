(in-package #:vacietis.libc.string.h)
(in-readtable vacietis)

(defun strerror (errnum)
  (aref vacietis.libc.errno.h::errno-strings errnum))

(eval-when (:compile-toplevel :load-toplevel)
  (load-c-file
   (merge-pathnames "string.c"
                    (directory-namestring
                     #.(or *load-truename* *compile-file-truename*)))))
