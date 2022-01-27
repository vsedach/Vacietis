(in-package #:vacietis)
(in-readtable vacietis)

(defun pp-defines (pkg)
  (intern "*PREPROCESSOR-DEFINES*" pkg))

(defmacro define (name value &rest docstring)
  "Like a #define but also visible to Lisp code."
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (defvar ,(pp-defines *package*) (make-hash-table))
       (setf (gethash ',name ,(pp-defines *package*)) ,(prin1-to-string value)))
     (defparameter ,name ,value ,@docstring)))

(defmacro defun/1 (name arglist &body body)
  "Lisp-1 defun; makes function pointers work."
  `(progn
     (defun ,name ,arglist
       ,@body)
     (defparameter ,name (vacietis.c:mkptr& (symbol-function ',name)))))

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
      (%load-c-file it *compiler-state*))))

(defmacro libc-dir ()
  (directory-namestring (or *compile-file-truename* *load-truename*)))

(defmacro load-libc-file (file libc-dir)
  `(eval-when (:compile-toplevel :load-toplevel)
     (load-c-file
      (merge-pathnames ,file ,libc-dir))))
