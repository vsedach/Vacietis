(in-package #:vacietis)
(in-readtable vacietis)

(in-package #:vacietis.c)

(cl:defparameter vacietis:*basic-c-types*
  '(int void short long float double char))

(cl:defparameter vacietis:*type-qualifiers*
  '(static const signed unsigned extern auto))

(cl:in-package #:vacietis)

(defun size-of (type)
  (if (consp type)
      (length (gethash (second type) *c-structs*))
      1))
