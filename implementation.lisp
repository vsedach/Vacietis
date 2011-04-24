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

;;; pointers, storage units and allocation

;;; pointers are represented by conses (they never occur as C types)
;;; an array pointer is a cons (array . index)
;;; a place pointer is a cons (closure . nil)

(defconstant vacietis.c:NULL 0)

;; (defmacro sizeof (x)
;;   (cond ((basic-type? x) 1)
;;         ((typedef? x) (typedef-size x))
;;         (t (let ((var (gensym)))
;;              `(let ((,var ,x))
;;                 (if (vectorp ,var)
;;                     (length ,var)
;;                     1))))))

(defun string-to-char* (string)
  (cons (let ((unicode (babel:string-to-octets string :encoding :utf-8)))
          (adjust-array unicode (1+ (length unicode)) :initial-element 0))
        0))

(defun array-literal (&optional (size 0) literal) ;; single dimension
  (cons (if literal
            (adjust-array literal (max size (length literal)) :initial-element 0)
            (make-array size :initial-element 0))
        0))

(defmacro vacietis.c:mkptr& (place) ;; need to deal w/function pointers
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

;;; arithmetic

;; things that operate on pointers: + - < > <= >= == != ++ -- !

;; may want to make these methods into cases in inlineable functions

(defmethod vacietis.c:+ ((x number) (y number))
  (+ x y))

(defmethod vacietis.c:+ ((ptr cons) (x integer))
  (cons (car ptr) (+ x (cdr ptr))))

(defmethod vacietis.c:+ ((x integer) (ptr cons))
  (cons (car ptr) (+ x (cdr ptr))))

(defmethod vacietis.c:- ((x number) (y number))
  (- x y))

(defmethod vacietis.c:- ((ptr cons) (x integer))
  (cons (car ptr) (- (cdr ptr) x)))

(defmethod vacietis.c:- ((ptr1 cons) (ptr2 cons))
  (assert (eq (car ptr1) (car ptr2)) ()
          "Trying to subtract pointers from two different memory segments")
  (cons (car ptr1) (- (cdr ptr1) (cdr ptr2))))

;;; comparison operators

(define-binary-op-mapping-definer define-comparison-ops
  `(progn (defmethod ,op ((x cons) (y cons))
            (if (and (eql (car x) (car y))
                     (,cl (cdr x) (cdr y)))
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
  `(progn ,@(loop for op in ops collect
                 `(defmacro ,(find-symbol (symbol-name op) '#:vacietis.c) (lvalue rvalue)
                    `(setf ,lvalue
                           (,',(find-symbol (reverse (subseq (reverse (symbol-name op)) 1))
                                            '#:vacietis.c)
                               ,lvalue
                               ,rvalue))))))

(unroll-assignment-ops += -= *= /= %= <<= >>= &= ^= |\|=|)

(defmacro vacietis.c:++ (x)
  `(vacietis.c:= ,x (vacietis.c:+ ,x 1)))
(defmacro vacietis.c:-- (x)
  `(vacietis.c:= ,x (vacietis.c:+ ,x 1)))
(defmacro vacietis.c:post++ (x)
  `(prog1 ,x (vacietis.c:++ ,x)))
(defmacro vacietis.c:post-- (x)
  `(prog1 ,x (vacietis.c:-- ,x)))

;;; iteration

(defmacro vacietis.c:for ((bindings initialization test increment) &body body)
  `(let ,bindings
     (tagbody ,@(awhen initialization (list it))
      loop
        (when (eql 0 ,test)
          (go break))
        ,@body
      continue
        ,@(awhen increment (list it))
        (go loop)
      break)))

(defmacro vacietis.c:while (test &body body)
  `(vacietis.c:for (nil nil ,test nil) ,@body))

(defmacro vacietis.c:do (test &body body)
  `(tagbody loop
      ,@body
    continue
      (if (eql 0 ,test)
          (go break)
          (go loop))
    break))

;;; control flow

(defmacro vacietis.c:if (test then-statements &rest clauses)
  `(if ,test
       (tagbody ,@then-statements)))

(defmacro vacietis.c:return (&optional value)
  `(return ,(or value 0)))

;;; declarations

(defmacro c-fun (name arglist vars &body body)
  `(defun ,name ,arglist
     (prog* ,vars ,@body)))
