(in-package #:vacietis)

(defstruct pointer
  type
  array
  index)

; ================================================================
; Old pointer code

;; (defmacro zcptr>array (ptr)
;;   "Returns the array part of a consed pointer."
;;   `(car ,ptr))

;; (defmacro zcptr>index (ptr)
;;   "Returns the index part of a consed pointer."
;;   `(cdr ,ptr))

;; (defmacro zcptr>cons (array index &optional reuse)
;;   "Conses a pointer to the INDEXth element of ARRAY.  If REUSE is provided, it is
;;    a zcptr to be modified to point to the new ARRAY and INDEX; except, if either
;;    the ARRAY or INDEX expression (not value) is :OLD, that cell of the pointer is
;;    not changed."
;;   (if reuse
;; 	 (let ((rplaca-form (if (eq array ':old) reuse `(rplaca ,reuse ,array))))
;; 	   (if (eq index `:old) rplaca-form `(rplacd ,rplaca-form ,index)))
;;     `(cons ,array ,index)))

;; (defsubst zcptr>ptr-p (frob)
;;   "Is FROB a C pointer?"
;;   (listp frob))

;; (defsubst zcptr>aref (array index)
;;   (aref array index))

;; (defsubst zcptr>aset (value array index)
;;   (aset value array index))

;; (#-Symbolics defsetf #+Symbolics cl:defsetf zcptr>aref (array index) (value)
;;   `(zcptr>aset ,value ,array ,index))

;; (defmacro zcptr>aref-s8b (array index)
;;   (zcprim>8-bit-sign-extend `(zcptr>aref ,array ,index)))

;; (defmacro zcptr>aset-s8b (value array index)
;;   `(zcptr>aset ,value ,array ,index))

;; (#-Symbolics defsetf #+Symbolics cl:defsetf zcptr>aref-s8b (array index) (value)
;;   `(zcptr>aset-s8b ,value ,array ,index))

;; (defmacro zcptr>aref-s16b (array index)
;;   (zcprim>16-bit-sign-extend `(zcptr>aref ,array ,index)))

;; (defmacro zcptr>aset-s16b (value array index)
;;   `(zcptr>aset ,value ,array ,index))

;; (#-Symbolics defsetf #+Symbolics cl:defsetf zcptr>aref-s16b (array index) (value)
;;   `(zcptr>aset-s16b ,value ,array ,index))

;; (defsubst zcptr>null-p (ptr.ar ptr.idx)
;;   (and (null ptr.ar) (zerop ptr.idx)))

;; ; This is so the code generator doesn't confuse itself.
;; (defmacro zcptr>flat-deref (ptr)
;;   ptr)

;; (defun zcptr>equal (arg1.ar arg1.idx arg2.ar arg2.idx)
;;   (and (eq arg1.ar arg2.ar)
;; 	  (eql arg1.idx arg2.idx)))

;; (defun zcptr>compare-check (array-1 array-2)
;;   (and (neq array-1 array-2)
;; 	  (or *compare-incomparable-pointers*
;; 		 (ferror "Can't compare pointers to different arrays ~A and ~A"
;; 				array-1 array-2))))

;; (defun zcptr>subtract-check (array-1 array-2)
;;   (or (eq array-1 array-2)
;; 	 (ferror "Can't subtract pointers into different arrays, ~A and ~A"
;; 			array-1 array-2)))
