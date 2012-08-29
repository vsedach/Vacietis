(in-package #:vacietis)
(in-readtable vacietis)

(in-package #:vacietis.c)

(cl:defparameter vacietis::*basic-c-types*
  '(int void short long float double char))

(cl:defparameter vacietis::*type-qualifiers*
  '(static const signed unsigned extern auto))

(cl:in-package #:vacietis)

;; these should at least be weak, but ideally only bound like *preprocessor-state*
(defvar *c-structs* (make-hash-table))
(defvar *variable-sizes* (list (make-hash-table)))

(defun variable-size (name)
  (loop for env in *variable-sizes* thereis (gethash name env)))

(defun size-of (x)
  (acond ((member x *basic-c-types*) 1)
         ((variable-size x)          it)
         ((gethash x *c-structs*)    (length it))))

;; (defmacro sizeof (x) ;; this is not used anywhere, for now
;;   (cond ((intersection x *basic-c-types*) 1)
;;         ((typedef? x) (typedef-size x))
;;         (t (let ((var (gensym)))
;;              `(let ((,var ,x))
;;                 (if (vectorp ,var)
;;                     (length ,var)
;;                     1))))))