(in-package #:vacietis)
(in-readtable vacietis)

(in-package #:vacietis.c)

(cl:defparameter vacietis:*basic-c-types*
  '(int void short long float double char))

(cl:defparameter vacietis:*type-qualifiers*
  '(static const signed unsigned extern auto))

(cl:in-package #:vacietis)

(defvar *c-structs* (make-hash-table)) ;; needs to be weak

;; right now just used to hold sizes
(defvar *variable-sizes* (list (make-hash-table))) ;; this table needs to be weak

(defun variable-size (name)
  (loop for env in *variable-sizes* thereis (gethash name env)))

(defun size-of (x)
  (acond ((member x *basic-c-types*) 1)
         ((variable-size x)          it)
         ((gethash x *c-structs*)    (length it))))
