(in-package #:vacietis)
(in-readtable vacietis)

(declaim (optimize (debug 3)))

(defmacro define-binary-op-mapping-definer (name &body body)
  `(defmacro ,name (&rest map)
     `(progn ,@(loop for (vacietis cl) on map by #'cddr collect
                    (let ((op (find-symbol (symbol-name vacietis) '#:vacietis.c)))
                      ,@body)))))

(define-binary-op-mapping-definer define-binary-ops
    `(progn (declaim (inline ,op))
            (defun ,op (x y)
              (,cl x y))))

(define-binary-ops
  |\|| logior
  ^    logxor
  &    logand
  *    *
  /    /
  %    rem
  )

(declaim (inline vacietis.c:~))
(defun vacietis.c:~ (x)
  (lognot x))

;;; pointers are represented by conses (they never occur as C types)
;;; an array pointer is a cons (array . index)
;;; a place pointer is a cons (closure . nil)

(defconstant vacietis.c:NULL 0)

(defmacro vacietis.c:mkptr& (place)
  (let ((new-value (gensym)))
    `(cons (lambda (&optional ,new-value) ;; this assumes nil doesn't occur as a C value
             (if ,new-value
                 (setf ,place ,new-value)
                 ,place))
           nil)))

(defun vacietis.c:deref* (ptr)
  (if (cdr ptr)
      (aref (car ptr) (cdr ptr))
      (funcall (car ptr))))

(defun (setf vacietis.c:deref*) (new-value ptr)
  (if (cdr ptr)
      (setf (aref (car ptr) (cdr ptr)) new-value)
      (funcall (car ptr) new-value)))

;; things that operate on pointers: + - < > <= >= == != ++ -- !

;; may want to make these methods into cases in inlineable functions

(defmethod vacietis.c:+ ((x number) (y number))
  (+ x y))

(defmethod vacietis.c:+ ((ptr cons) (x integer))
  (cons (car ptr) (+ x (cdr ptr))))

(defmethod vacietis.c:+ ((x integer) (ptr cons))
  (vacietis.c:+ ptr x))

(defmethod vacietis.c:- ((x number) (y number))
  (- x y))

(defmethod vacietis.c:- ((ptr cons) (x integer))
  (cons (car ptr) (- (cdr ptr) x)))

(defmethod vacietis.c:- ((ptr1 cons) (ptr2 cons))
  (assert (eq (car ptr1) (car ptr2)) () "Trying to subtract pointers from two different memory regions")
  (cons (car ptr1) (- (cdr ptr1) (cdr ptr2))))

(define-binary-op-mapping-definer define-array-ptr-ops
    `(defmethod ,op ((x cons) (y cons))
       (if (and (eql (car x) (car y))
                (,cl (cdr x) (cdr y)))
           1
           0)))

(define-array-ptr-ops
  == =
  <  <
  >  >
  <= <=
  >= >=)

(defmethod vacietis.c:== (x y)
  0)

(defmethod vacietis.c:== ((x number) (y number))
  (if (= x y) 1 0))

(declaim (inline vacietis.c:!))
(defun vacietis.c:! (x)
  (vacietis.c:== x 0))

(declaim (inline vacietis.c:!=))
(defun vacietis.c:!= (x y)
  (vacietis.c:! (vacietis.c:== x y)))
