(in-package #:vacietis.libc.stdarg.h)
(in-readtable vacietis)

(defmacro va_start (va-list-var last-fixed-parameter-name)
  (declare (ignore last-fixed-parameter-name))
  `(setf ,va-list-var (mkptr& vacietis.c:|...|)))

(defun/1 va_end (va-list) ;; does nothing
  (declare (ignore va-list)))

(defun __va_arg (va-list)
  (or (pop (deref* va-list))
      (error "va_arg just corrupted the stack")))

(defun/1 va_copy (dest src)
  (setf (deref* dest) (deref* src)))
