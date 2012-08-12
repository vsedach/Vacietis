(in-package #:vacietis)
(in-readtable vacietis)

(defvar *c-structs* (make-hash-table))

;; right now just used to hold sizes
(defvar *variable-types* (list (make-hash-table)))

(defun variable-info (name)
  (loop for env in *variable-types* thereis (gethash name env)))
