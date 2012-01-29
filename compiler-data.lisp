(in-package #:vacietis)
(in-readtable vacietis)

(in-package #:vacietis.c)

(cl:defparameter vacietis:*basic-c-types*
  '(int void short long float double char))

(cl:in-package #:vacietis)

(defvar *c-structs* (make-hash-table))

(defvar *variable-types* (list (make-hash-table)))

(defun variable-info (name)
  (loop for env in *variable-types* thereis (gethash name env)))
