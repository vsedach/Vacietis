(in-package #:vacietis)
(in-readtable vacietis)

(in-package #:vacietis.c)

(cl:defparameter vacietis::*basic-c-types*
  #(int void short long float double char))

(cl:in-package #:vacietis)

(defstruct c-type)

(defstruct (pointer-to (:include c-type))
  type)

(defstruct (enum-type (:include c-type))
  name)

(defstruct (struct-type (:include c-type))
  name
  slots)

(defstruct (array-type (:include c-type))
  element-type
  dimensions)

(defun type-size (type)
  (cond
    ((find type *basic-c-types*) 1)
    ((enum-type-p   type)        1)
    ((pointer-to-p  type)        1)
    ((struct-type-p type)        (reduce #'+ (map 'list #'type-size
                                                  (struct-type-slots type))))
    ((array-type-p type)         (if (array-type-dimensions type)
                                     (apply #'*
                                            (type-size
                                             (array-type-element-type type))
                                            (array-type-dimensions type))
                                     (error
                                      "Array has no dimensions specified")))))

(defun preallocated-value-exp-for (type)
  (cond
    ((struct-type-p type)  `(make-array ,(size-of type) :initial-element 0))
    ((array-type-p type)   `(allocate-memory ,(size-of type)))
    (t                     0)))
