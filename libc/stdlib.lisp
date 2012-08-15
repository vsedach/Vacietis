(in-package #:vacietis.libc.stdlib.h)
(in-readtable vacietis)

;;; allocation

(defun malloc (size)
  (if (= size 0)
      NULL
      (allocate-memory size)))

(defun calloc (count size)
  (malloc (* count size)))

(defun realloc (memory size)
  (cond ((eql memory NULL)
         (malloc size))
        ((= size 0)
         (free memory)
         NULL)
        (t
         (adjust-array (memptr-mem memory) size :initial-element 0)
         memory)))

(defun free (memory)
  (unless (eql NULL memory)
    (setf (memptr-mem memory) 'FREED_BY_FREE)))

;;; random numbers

(defconstant RAND_MAX most-positive-fixnum)

(defun rand ()
  (random RAND_MAX))

(defun srand (state) ;; todo
  (error "FIGURE OUT WHAT TO DO W/RANDOM STATES")
  (setf *random-state* state))

;;; numeric conversion

(defun atoi (str)
  (parse-integer (char*-to-string str) :junk-allowed t))

(defun atol (str)
  (atoi str))

(defun atoll (str)
  (atoi str))

(defun atof (str)
  (let* ((str   (char*-to-string str))
         (start (position-if (lambda (c)
                               (not (find c '(#\Space #\Tab #\Newline))))
                             str)))
    (read-from-string
     str
     :start start
     :end   (position-if (lambda (c)
                           (not (find c ".0123456789+-eE")))
                         str
                         :start start))))

(defun strtod (str end-ptr)
  (multiple-value-bind (number end)
      (read-from-string (char*-to-string str))
    (if (numberp number)
        (progn
          (unless (eql end-ptr NULL)
            (setf (deref* end-ptr) (vacietis.c:+ str end)))
          number)
        (progn
          (setf (deref* end-ptr) str
                errno            ERANGE)
          0))))

(defun strtof (a b)
  (strtod a b))

(defun strtold (a b)
  (strtod a b))

(defun strtol (str end-ptr base)
  (multiple-value-bind (number end)
      ;; fixme for octals
      (parse-integer (char*-to-string str)
                     :radix (if (= base 0)
                                10 ;; should be auto
                                base)
                     :junk-allowed t)
    (if (integerp number)
        (progn
          (unless (eql end-ptr NULL)
            (setf (deref* end-ptr) (vacietis.c:+ str end)))
          number)
        (progn
          (setf (deref* end-ptr) str
                errno            ERANGE)
          0))))

(defun strtoll (a b c)
  (strtol a b c))

(defun strtoul (a b c)
  (strtol a b c))

(defun strtoull (a b c)
  (strtol a b c))

;;; program environment

(defconstant EXIT_SUCCESS 0)
(defconstant EXIT_FAILURE 1)

(defun abort ()
  (throw 'vacietis::c-exit EXIT_FAILURE))

(defvar *exit-functions* ())

(defun exit (status)
  (dolist (f *exit-functions*)
    (funcall f))
  ;; close streams
  ;; delete tmpfiles
  (throw 'vacietis::c-exit status))

(defun atexit (f)
  (push f *exit-functions*))

(defun getenv (name)
  (gethash name vacietis::*environment* NULL))

(defun setenv (name value)
  (setf (gethash name vacietis::*environment*) value))

(defun system (command)
  0)

;;; search and sort
;;; get bsearch and qsort from a reference libc

;;; some math functions

(defun labs (x)
  (abs x))

;; need structs for div/ldiv
