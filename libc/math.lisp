(in-package #:vacietis.libc.math.h)
(in-readtable vacietis)

(define HUGE_VAL most-positive-double-float)

(defun atan2 (x y)
  (atan x y))

(defun pow (x y)
  (expt x y))

(defun log10 (x)
  (log x 10))

(defun fabs (x)
  (abs x))

(defun ceil (x)
  (ceiling x))

(defun fmod (x y)
  (mod x y))

(defun modf (x ptr)
  (multiple-value-bind (whole part)
      (fround x)
    (setf (deref* ptr) whole)
    part))

(defun ldexp (x n)
  (* x (expt 2 n)))

(defun frexp (x nptr)
  (let ((n (ceiling (log (abs x) 2))))
    (setf (deref* nptr) n)
    (/ x (expt 2 n))))
