(in-package #:vacietis.libc.math.h)
(in-readtable vacietis)

(define HUGE_VAL most-positive-double-float)

(defun/1 atan2 (x y)
  (atan x y))

(defun/1 pow (x y)
  (expt x y))

(defun/1 log10 (x)
  (log x 10))

(defun/1 fabs (x)
  (abs x))

(defun/1 ceil (x)
  (ceiling x))

(defun/1 fmod (x y)
  (mod x y))

(defun/1 modf (x ptr)
  (multiple-value-bind (whole part)
      (fround x)
    (setf (deref* ptr) whole)
    part))

(defun/1 ldexp (x n)
  (* x (expt 2 n)))

(defun/1 frexp (x nptr)
  (let ((n (ceiling (log (abs x) 2))))
    (setf (deref* nptr) n)
    (/ x (expt 2 n))))
