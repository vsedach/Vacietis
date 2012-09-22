(in-package #:vacietis.libc.stdlib.h)
(in-readtable vacietis)

;;; allocation

(defun/1 malloc (size)
  (if (= size 0)
      NULL
      (allocate-memory size)))

(defun/1 calloc (count size)
  (malloc (* count size)))

(defun/1 realloc (memory size)
  (cond ((eql memory NULL)
         (malloc size))
        ((= size 0)
         (free memory)
         NULL)
        (t
         (adjust-array (memptr-mem memory) size :initial-element 0)
         memory)))

(defun/1 free (memory)
  (unless (eql NULL memory)
    (setf (memptr-mem memory) 'FREED_BY_FREE)))

;;; random numbers

(defconstant RAND_MAX most-positive-fixnum)

(defun/1 rand ()
  (random RAND_MAX))

(defun/1 srand (state) ;; todo
  (error "FIGURE OUT WHAT TO DO W/RANDOM STATES")
  (setf *random-state* state))

;;; numeric conversion

(defun/1 atoi (str)
  (parse-integer (char*-to-string str) :junk-allowed t))

(defun/1 atol (str)
  (atoi str))

(defun/1 atoll (str)
  (atoi str))

(defun/1 atof (str)
  (let* ((str   (char*-to-string str))
         (start (position-if (lambda (c)
                               (not (find c '(#\Space #\Tab #\Newline))))
                             str)))
    (read-from-string
     str
     nil
     nil
     :start start
     :end   (position-if (lambda (c)
                           (not (find c ".0123456789+-eE")))
                         str
                         :start start))))

(defun/1 strtod (str end-ptr)
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

(defun/1 strtof (a b)
  (strtod a b))

(defun/1 strtold (a b)
  (strtod a b))

(defun/1 strtol (str end-ptr base)
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

(defun/1 strtoll (a b c)
  (strtol a b c))

(defun/1 strtoul (a b c)
  (strtol a b c))

(defun/1 strtoull (a b c)
  (strtol a b c))

;;; program environment

(define EXIT_SUCCESS 0)
(define EXIT_FAILURE 1)

(defun/1 abort ()
  (throw 'vacietis::c-exit EXIT_FAILURE))

(defvar *exit-functions* ())

(defun/1 exit (status)
  (dolist (f *exit-functions*)
    (funcall f))
  ;; close streams
  ;; delete tmpfiles
  (throw 'vacietis::c-exit status))

(defun/1 atexit (f)
  (push f *exit-functions*))

(defun/1 getenv (name)
  (gethash name vacietis::*environment* NULL))

(defun/1 setenv (name value)
  (setf (gethash name vacietis::*environment*) value))

(defun/1 system (command) ;; this could be eval
  (declare (ignore command))
  0)

;;; math functions

(defun/1 labs (x)
  (abs x))

(defun/1 llabs (x)
  (abs x))

(defun/1 div (n d)
  )

(defun/1 ldiv (n d)
  (div n d))

(defun/1 lldiv (n d)
  (div n d))

;;; search and sort
;;; TODO
