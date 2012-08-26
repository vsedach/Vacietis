(in-package #:vacietis)
(in-readtable vacietis)

(declaim (optimize (debug 3)))

;;; unary operators

(defmacro def-unary-op (c lisp)
  (let ((x (gensym)))
   `(defmacro ,c (,x)
      `(,',lisp ,,x))))

;;; binary arithmetic

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

(def-unary-op vacietis.c:~ lognot)

(defmacro sizeof (x) ;; this is not used anywhere, for now
  (cond ((intersection x vacietis.reader::*basic-c-types*) 1)
        ((typedef? x) (typedef-size x))
        (t (let ((var (gensym)))
             `(let ((,var ,x))
                (if (vectorp ,var)
                    (length ,var)
                    1))))))

;;; pointers, storage units and allocation

(defstruct memptr
  mem
  (ptr 0))

(defun string-to-char* (string)
  (make-memptr
   :mem (let ((unicode (babel:string-to-octets string :encoding :utf-8)))
          (adjust-array unicode (1+ (length unicode)) :initial-element 0))))

(defun char*-to-string (char*)
  (let ((mem   (memptr-mem char*))
        (start (memptr-ptr char*)))
   (babel:octets-to-string mem
                           :encoding :utf-8
                           :start start
                           :end (position 0 mem :start start))))

(defun allocate-memory (size)
  (make-memptr :mem (make-array size :adjustable t :initial-element 0)))

(defstruct place-ptr
  closure)

(defmacro vacietis.c:mkptr& (place) ;; need to deal w/function pointers
  (let ((new-value (gensym)))
    `(make-place-ptr :closure (lambda (&optional ,new-value)
                                (if ,new-value
                                    (setf ,place ,new-value)
                                    ,place)))))

(defun vacietis.c:deref* (ptr)
  (etypecase ptr
    (memptr    (aref (memptr-mem ptr) (memptr-ptr ptr)))
    (place-ptr (funcall (place-ptr-closure ptr)))))

(defun (setf vacietis.c:deref*) (new-value ptr)
  (etypecase ptr
    (memptr    (setf (aref (memptr-mem ptr) (memptr-ptr ptr)) new-value))
    (place-ptr (funcall (place-ptr-closure ptr) new-value))))

(defun vacietis.c:[] (a i)
  (aref (memptr-mem a) (+ (memptr-ptr a) i)))

(defun (setf vacietis.c:[]) (new-value a i)
  (setf (aref (memptr-mem a) (+ (memptr-ptr a) i)) new-value))

;;; arithmetic

;; things that operate on pointers: + - < > <= >= == != ++ -- !

;; may want to make these methods into cases in inlineable functions

(defmethod vacietis.c:+ ((x number) (y number))
  (+ x y))

(defmethod vacietis.c:+ ((ptr memptr) (x integer))
  (make-memptr :mem (memptr-mem ptr) :ptr (+ x (memptr-ptr ptr))))

(defmethod vacietis.c:+ ((x integer) (ptr memptr))
  (vacietis.c:+ ptr x))

(defmethod vacietis.c:- ((x number) (y number))
  (- x y))

(defmethod vacietis.c:- ((ptr memptr) (x integer))
  (make-memptr :mem (memptr-mem ptr) :ptr (- (memptr-ptr ptr) x)))

(defmethod vacietis.c:- ((ptr1 memptr) (ptr2 memptr))
  (assert (eq (memptr-mem ptr1) (memptr-mem ptr2)) ()
          "Trying to subtract pointers from two different memory segments")
  (- (memptr-ptr ptr1) (memptr-ptr ptr2)))

;;; comparison operators

(define-binary-op-mapping-definer define-comparison-ops
  `(progn (defmethod ,op ((x memptr) (y memptr))
            (if (and (eq  (memptr-mem x) (memptr-mem y))
                     (,cl (memptr-ptr x) (memptr-ptr y)))
                1
                0))
          (defmethod ,op ((x number) (y number))
            (if (,cl x y) 1 0))))

(define-comparison-ops
  == =
  <  <
  >  >
  <= <=
  >= >=)

(defmethod vacietis.c:== (x y)
  (declare (ignore x y))
  0)

;;; boolean algebra

(declaim (inline vacietis.c:!))
(defun vacietis.c:! (x)
  (if (eql x 0) 1 0))

(declaim (inline vacietis.c:!=))
(defun vacietis.c:!= (x y)
  (vacietis.c:! (vacietis.c:== x y)))

(defmacro vacietis.c:&& (a b)
  `(if (or (eql ,a 0) (eql ,b 0)) 0 1))

(defmacro vacietis.c:|\|\|| (a b)
  `(if (or (not (eql ,a 0)) (not (eql ,b 0))) 1 0))

;;; assignment

(defmacro vacietis.c:= (lvalue rvalue)
  `(setf ,lvalue ,rvalue))

(defmacro unroll-assignment-ops (&rest ops)
  `(progn
     ,@(loop for op in ops collect
            `(defmacro ,(find-symbol (symbol-name op) '#:vacietis.c)
                 (lvalue rvalue)
               `(setf ,lvalue
                      (,',(find-symbol
                           (reverse (subseq (reverse (symbol-name op)) 1))
                           '#:vacietis.c)
                          ,lvalue
                          ,rvalue))))))

(unroll-assignment-ops += -= *= /= %= <<= >>= &= ^= |\|=|)

;;; iteration

(defmacro vacietis.c:for ((bindings initialization test increment) body)
  `(let ,bindings
     (tagbody ,@(awhen initialization (list it))
      loop
        (when (eql 0 ,test)
          (go break))
        ,body
      continue
        ,@(awhen increment (list it))
        (go loop)
      break)))

(defmacro vacietis.c:do (body test)
  `(tagbody loop
      ,body
    continue
      (if (eql 0 ,test)
          (go break)
          (go loop))
    break))

;;; structs

(defmacro c-struct (name slots)
  ;; DEFINE ACCESSORS!!!
  )

;;; literals

(defun literal (x)
  (etypecase x
    (string (string-to-char* x))
    (array  (make-memptr :mem x))))
