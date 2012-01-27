(in-package #:vacietis.stdlib)
(in-readtable vacietis)

;;; allocation

(defun malloc (size)
  (if (= size 0)
      NULL
      (array-literal size)))

(defun calloc (count size)
  (malloc (* count size)))

(defun realloc (memory size)
  (cond ((eql memory NULL) (malloc size))
        ((= size 0) (setf (car memory) 'FREED_BY_REALLOC) memory)
        (t (adjust-array (car memory) size :initial-element 0) memory)))

(defun free (memory)
  (unless (eql NULL memory)
    (setf (car memory) 'FREED_BY_FREE)))

;;; random numbers

(defconstant RAND_MAX most-positive-fixnum)

(defun rand ()
  (random RAND_MAX))

(defun srand (state)
  (setf *random-state* state))

;;; numeric conversion

(defun atoi (str)
  (parse-integer (char*-to-string str) :junk-allowed t))

(defun atol (str)
  (atoi str))

(defun atoll (str)
  (atoi str))

(defun atof (str)
  )

(defun strtod (str end-ptr)
  (multiple-value-bind (number end)
      (read-from-string (char*-to-string str))
    (if (numberp number)
        (progn
          (unless (eql end NULL)
            (setf (deref* end-ptr) (vacietis.c:+ str end)))
          number)
        'error))) ;; fixme

(defun strtof (a b)
  (strtod a b))

(defun strtold (a b)
  (strtod a b))

;;; program environment

;;; search and sort

;;; string conversion

