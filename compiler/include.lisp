(in-package #:vacietis.reader)
(in-readtable vacietis)

(defvar vacietis.reader::*preprocessor-defines* nil)

(defun pp-defines (&optional (pkg *package*))
  (intern "*PREPROCESSOR-DEFINES*" pkg))

(defmacro define (name value &rest docstring)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (defvar ,(pp-defines) (make-hash-table))
       (setf (gethash ',name ,(pp-defines)) ,(prin1-to-string value)))
     (defparameter ,name ,value ,@docstring)))

(defun include-libc-file (include-file)
  (let ((libc-package (find-package (format nil "VACIETIS.LIBC.~:@(~A~)"
                                            include-file))))
    (use-package libc-package)
    (awhen (and (boundp       (pp-defines libc-package))
                (symbol-value (pp-defines libc-package)))
      (maphash (lambda (name expansion)
                 (setf (gethash (intern (symbol-name name) *package*)
                                vacietis.reader::*preprocessor-defines*)
                       expansion))
               it))))
