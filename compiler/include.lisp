(in-package #:vacietis)
(in-readtable vacietis)

(defun pp-defines (pkg)
  (intern "*PREPROCESSOR-DEFINES*" pkg))

(defmacro define (name value &rest docstring)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (defvar ,(pp-defines *package*) (make-hash-table))
       (setf (gethash ',name ,(pp-defines *package*)) ,(prin1-to-string value)))
     (defparameter ,name ,value ,@docstring)))

(defun include-libc-file (include-file)
  (let ((libc-package (find-package (format nil "VACIETIS.LIBC.~:@(~A~)"
                                            include-file))))
    (unless libc-package
      (error "Can't find libc include file ~a" include-file))
    (use-package libc-package)
    (awhen (and (boundp       (pp-defines libc-package))
                (symbol-value (pp-defines libc-package)))
      (maphash (lambda (name expansion)
                 (setf (gethash (intern (symbol-name name) *package*)
                                (compiler-state-pp *compiler-state*))
                       expansion))
               it))
    (awhen (probe-file
            (merge-pathnames
             (format nil "../libc/~a" include-file)
             #.(or *compile-file-truename* *load-truename*)))
      (vacietis.reader::%load-c-file it *compiler-state*))))
