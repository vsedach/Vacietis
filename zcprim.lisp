; -*- Mode: Lisp; Package: Zeta-C; Base: 10 -*-
; File: ZCPRIM.LISP
;
;    This code has been placed in the public domain.
;
; This file contains the definitions of the Zeta-C primitives.


(defvar zcprim>*expanding-defunc+* nil
  "T when a defunc+ expansion is in process.")

(defvar zcprim>*defun-specials* :unbound
  "A list of variables to be declared special, that's being accumulated for the
   top-level defun.")

(defvar zcprim>*defun-static-inits* :unbound
  "A list of initialization clauses for statics, that's being accumulated for the
   top-level defun.")

(defvar zcprim>*defun-cleanup-forms* :unbound
  "A list of forms to do at the end of the function, with an unwind-protect even.")

(defvar zcprim>*defun-toplevel-forms* :unbound
  "A list of miscellaneous things to do at top level of a defun.")

(defvar zcprim>*defun-function-call-info* :unbound
  "A list of lists of the form
     (caller callee expected-return-type . arg-types-passed)
   telling how the function has been called from this function.")

(defprim c:+
  "Arithmetic addition.  Also, one arg may be a pointer.  Or, the second arg may be
   omitted (the ANSI spec defines unary + to constrain the order of evaluation of
   arithmetic expressions; we don't rearrange expressions anyway, so just ignore
   it)."
  (((arg1 type1 :unary))
   (values arg1 (zcprim>unary-arith-result "+ (unary)" type1)))
  (((arg1 type1 :binary) (arg2 type2 :binary))
   (cond ((and (zctype>number-p type1) (zctype>number-p type2))
	  (values (zcprim>+ arg1 arg2)
		  (zcprim>binary-arith-result "+" type1 type2)))
	 ((and (zctype>arith-pointer-p type1) (zctype>integer-p type2))
	  (zcprim>pointer-plus-int arg1 arg2 type1 #'zcprim>+ **env))
	 ((and (zctype>integer-p type1) (zctype>arith-pointer-p type2))
	  (zcprim>pointer-plus-int arg2 arg1 type2 #'zcprim>+ **env))
	 (t (zcerror "Wrong argument type to +: ~A or ~A" type1 type2)))))

(defprim c:-
  "Arithmetic subtraction.  The first arg may be a pointer.  Alternatively, both
   args may be pointers, but they must point into the same array (this constraint
   is enforced at runtime).  Or, the second arg may be omitted, for unary
   negation."
  (((arg1 type1 :unary))
   (values (zcprim>- arg1) (zcprim>unary-arith-result "- (unary)" type1)))
  (((arg1 type1 :binary) (arg2 type2 :binary))
   (cond ((and (zctype>number-p type1) (zctype>number-p type2))
	  (values (zcprim>- arg1 arg2)
		  (zcprim>binary-arith-result "- (binary)" type1 type2)))
	 ((and (zctype>arith-pointer-p type1) (zctype>integer-p type2))
	  (zcprim>pointer-plus-int arg1 arg2 type1 #'zcprim>- **env))
	 ((and (zctype>arith-pointer-p type1) (zctype>arith-pointer-p type2))
	  (zcprim>pointer-subtract arg1 type1 arg2 type2 **env))
	 (t (zcerror "Wrong argument type to -: ~A or ~A" type1 type2)))))

(defun zcprim>pointer-plus-int (arg1 arg2 type1 operation env)
  "Handles addition and subtraction of a pointer and an integer.  OPERATION should
   be #'ZCPRIM>+ or #'ZCPRIM>-."
  (nlet ((let-clauses ptr-array ptr-index
		      (zcprim>analyze-pointer-exp arg1 type1)))
    (values (zcprim>let-form let-clauses
	      `(zcptr>cons ,ptr-array
			   ,(funcall operation ptr-index
				     (zcprim>scale-ptr-offset arg2 type1 env))))
	    (zctype>canonicalize-pointer type1))))

(defun zcprim>pointer-subtract (arg1 type1 arg2 type2 env)
  "Handles the subtraction of two pointers."
  (nlet ((let-clauses-1 array-1 index-1
			(zcprim>analyze-pointer-exp arg1 type1))
	 (let-clauses-2 array-2 index-2
			(zcprim>analyze-pointer-exp arg2 type2)))
    (if (not (and (zctype>arith-pointer-p type1) (zctype>arith-pointer-p type2)
		  (zctype>match type1 type2 env)))
	(zcerror "Wrong argument type in pointer subtraction: ~A or ~A"
		 type1 type2)
      (values (zcprim>let-form (append let-clauses-1 let-clauses-2)
		`(if (zcptr>subtract-check ,array-1 ,array-2)
		     ,(zcprim>unscale-ptr-difference (zcprim>- index-1 index-2)
						     type1 env)))
	      (zctype>int)))))

(defprim c:*
  "Arithmetic multiplication.  Or, with one argument, pointer dereferencing."
  (((arg1 type1 :unary))
   (cond ((not (zctype>pointer-p type1))
	  (zcerror "Wrong argument type to * (pointer dereference): ~A" type1))
	 ; Special case for pointer-to-function deref.
	 ((zctype>function-pointer-p type1)
	  (values arg1 (zctype>pointer-deref-type type1)))
	 (t
	  (zcprim>deref-pointer arg1 type1 **env))))
  (((arg1 type1 :binary) (arg2 type2 :binary))
   (values (zcprim>* arg1 arg2)
	   (zcprim>binary-arith-result "* (multiply)" type1 type2))))

(defun zcprim>deref-pointer (ptr type env &optional unpacked-struct-reference)
  "Creates an expression to dereference a pointer expression PTR, of type TYPE.
   Returns that as its first value, and the type of that as its second."
  (nlet ((dtype (zctype>pointer-deref-type type))
	 ((let-clauses array index (zcprim>analyze-pointer-exp ptr type))))
    (values
      (cond ((and (listp array) (eq (car array) 'zcprim>address-var) (eql index 0)
		  ;; The following is to make sure a cast hasn't intervened.
		  (zctype>equal type (zcenv>type (cadr array) env)))
	     ;; Special case for "*&variable".
	     (cadr array))
	    ((or (zctype>struct-p dtype) (zctype>array-p dtype))
	     ;; Special case for pointer-to-aggregate deref in flat mode.
	     (zcprim>let-form let-clauses
	       `(zcptr>flat-deref (zcptr>cons ,array ,index))))
	    ;; Keep track of references to cells in unpacked structs, as they
	    ;; require shorten-on-store just like variables.
	    (unpacked-struct-reference
	     (zcprim>let-form let-clauses `(aref ,array ,index)))
	    (t (zcprim>let-form let-clauses (zcprim>aref array index dtype))))
      dtype)))

(defun zcprim>aref (array index type)
  (cond ((zctype>signed-char-p type)
	 `(zcptr>aref-s8b ,array ,index))
	((zctype>signed-short-p type)
	 `(zcptr>aref-s16b ,array ,index))
	(t `(zcptr>aref ,array ,index))))

(defun zcprim>aref-to-aset (aref-sym)
  (or (cdr (assq aref-sym '((aref . aset)
			    (zcptr>aref . zcptr>aset)
			    (zcptr>aref-s8b . zcptr>aset-s8b)
			    (zcptr>aref-s16b . zcptr>aset-s16b))))
      (zcerror "Internal error: Flavor of AREF, ~A, not accounted for!?"
	       aref-sym)))

(defprim c://
  "Arithmetic division.  Negative results of integer division truncate toward 0."
  (((arg1 type1 :binary) (arg2 type2 :binary))
   (values (zcprim>// arg1 arg2)
	   (zcprim>binary-arith-result "//" type1 type2))))

(defprim c:%
  "Arithmetic remainder.  Result has same sign as the dividend."
  (((arg1 type1 :binary) (arg2 type2 :binary))
   (values (zcprim>\ arg1 arg2)
	   (zcprim>binary-int-arith-result "%" type1 type2))))

(defprim c:<<
  "Logical left shift."
  (((arg1 type1 :unary) (arg2 type2 :unary))
   (values (zcprim>ash arg1 arg2)
	   (if (not (and (zctype>integer-p type1) (zctype>integer-p type2)))
	       (zcerror "Wrong argument type to <<: ~A or ~A" type1 type2)
	     type1))))

(defprim c:>>
  "Arithmetic right shift."
  (((arg1 type1 :unary) (arg2 type2 :unary))
   (values (zcprim>ash arg1 (zcprim>- arg2))
	   (if (not (and (zctype>integer-p type1) (zctype>integer-p type2)))
	       (zcerror "Wrong argument type to <<: ~A or ~A" type1 type2)
	     type1))))

(defprim c:&
  "Unary: pointer creation (address-of); binary: bitwise AND."
  (((arg type))
   (zcprim>pointer-to arg type **env **form))
  (((arg1 type1 :binary) (arg2 type2 :binary))
   (values (or (zcprim>&-ldb-optimization arg1 arg2)
	       (zcprim>&-ldb-optimization arg2 arg1)
	       (zcprim>logand arg1 arg2))
	   (zcprim>binary-int-arith-result "& (bitwise AND)" type1 type2))))

(defun zcprim>pointer-to (exp type env form)
  "Makes a pointer to expression EXP, of type TYPE."
  (bcond ((zctype>array-p type)
	  (zcwarn "Redundant & operator applied to array ~A" exp)
	  (values (zcprim>canonicalize-if-needed exp type)
		  (zctype>canonicalize-pointer type)))
	 ((and (zctype>function-pointer-p type) (listp exp)
	       (eq (car exp) 'zcprim>function))
	  (zcwarn "Redundant & operator applied to function name ~A" (cadr exp))
	  (values exp type))
	 ((zctype>struct-p type)
	  ;; Special case for structures in flat mode.
	  (values (zcprim>canonicalize-if-needed exp
						 (zctype>array-of (zctype>int)))
		  (zctype>pointer-to type)))
	 (((let-clauses array index arefs-p
	    (and (zcprim>lvalue-ok-check exp type (cadr form))
		 (zcprim>analyze-aref-exp exp type)))
	   arefs-p)			     ; special case for &foo[i], &*foo
	  (values (zcprim>let-form let-clauses `(zcptr>cons ,array ,index))
		  (zctype>pointer-to type)))
	 ((symbolp exp)
	  (values `(zcptr>cons ,(zcprim>variable-address exp env) 0)
		  (zctype>pointer-to type)))
	 ((and (listp exp) (memq (car exp) '(ldb ldb-signed)))
	  (zcerror "Cannot make a pointer to a bit field"))
	 (t (zcerror "Internal error: don't know how to make a pointer to ~A"
		     exp))))

(defun zcprim>variable-address (sym env)
  (nlet ((ignore depth (zcenv>type sym env)))
    (when (> depth 0)
      (zcenv>annotate sym 'address-taken t env)))
  `(zcprim>address-var ,sym))

; We put off translating this into sym.ADDRESS so the expression won't look like
; an lvalue.
(defmacro zcprim>address-var (sym)
  (zcprim>variable-address-var sym))

(defun zcprim>&-ldb-optimization (arg1 arg2)
  "If ARG1 is a fixnum of the form 2^n-1, and ARG2 is a form `(ash ,foo (- ,bar)),
   then instead of taking LOGAND of the two we can do an LDB out of FOO."
  (and (fixnump arg1) (< (haulong arg1) 24.) (= arg1 (1- (^ 2 (haulong arg1))))
       (listp arg2) (eq (car arg2) 'ash)
       (let ((ash-arg2 (caddr arg2)))
	 (and (listp ash-arg2) (eq (car ash-arg2) '-) (null (cddr ash-arg2))
	      (cond ((and (fixp (cadr ash-arg2)) (fixp (cadr arg2)))
		     (load-byte (cadr arg2) (cadr ash-arg2) (haulong arg1)))
		    (t `(ldb (byte ,(haulong arg1) ,(cadr ash-arg2))
			     ,(cadr arg2))))))))

(defprim c:/|
  "Bitwise OR."
  (((arg1 type1 :binary) (arg2 type2 :binary))
   (values (zcprim>logior arg1 arg2)
	   (zcprim>binary-int-arith-result "/| (bitwise OR)" type1 type2))))

(defprim c:^
  "Bitwise XOR."
  (((arg1 type1 :binary) (arg2 type2 :binary))
   (values (zcprim>logxor arg1 arg2)
	   (zcprim>binary-int-arith-result "^ (bitwise XOR)" type1 type2))))

(defprim c:~
  "Bitwise NOT, a.k.a. ones complement."
  (((arg1 type1 :unary))
   (values (zcprim>lognot arg1)
	   (zcprim>unary-int-arith-result "~ (bitwise NOT)" type1))))


; ================================================================
; Comparison and logical operators.

(defprim c:==
  "Equality comparison."
  (((arg1 type1 :binary) (arg2 type2 :binary))
   (zcprim>compare arg1 type1 arg2 type2 **env 'eql t "==")))

(defprim c:!=
  "Inequality comparison."
  (((arg1 type1 :binary) (arg2 type2 :binary))
   (zcprim>compare arg1 type1 arg2 type2 **env 'eql nil "!=")))

(defprim c:<
  "Less-than comparison."
  (((arg1 type1 :binary) (arg2 type2 :binary))
   (zcprim>compare arg1 type1 arg2 type2 **env '< t "<")))

(defprim c:>
  "Greater-than comparison."
  (((arg1 type1 :binary) (arg2 type2 :binary))
   (zcprim>compare arg2 type2 arg1 type1 **env '< t ">")))

(defprim c:<=
  "Less-than-or-equal-to comparison."
  (((arg1 type1 :binary) (arg2 type2 :binary))
   (zcprim>compare arg2 type2 arg1 type1 **env '< nil "<=")))

(defprim c:>=
  "Greater-than-or-equal-to comparison."
  (((arg1 type1 :binary) (arg2 type2 :binary))
   (zcprim>compare arg1 type1 arg2 type2 **env '< nil ">=")))

(defun zcprim>compare (arg1 type1 arg2 type2 env pred polarity pred-name)
  (when (not (memq pred '(eql <)))
    (zcerror "Internal error: pred ~A no longer allowed" pred))
  (cond ((and (zctype>number-p type1) (zctype>number-p type2))
	 (values (zcprim>apply-polarity
		   polarity
		   (cond ((and (zctype>unsigned-p type1) (eq pred '<))
			  (zcprim>unsigned-< arg1 arg2))
			 ((and (numberp arg1) (numberp arg2))
			  (funcall pred arg1 arg2))
			 (t `(,pred ,arg1 ,arg2))))
		 (zctype>boolean)))
	((or (and (zctype>function-pointer-p type1)
		  (or (zctype>function-pointer-p type2) (zctype>zero-p type2)))
	     (and (zctype>zero-p type1) (zctype>function-pointer-p type2)))
	 (if (neq pred 'eql)
	     (zcerror "Pointers to functions cannot be compared with ~A" pred-name)
	   (let ((arg1 (if (zctype>zero-p type1) (zcprim>null-function-pointer)
			 arg1))
		 (arg2 (if (zctype>zero-p type2) (zcprim>null-function-pointer)
			 arg2)))
	     (values (zcprim>apply-polarity polarity (if (eq arg1 arg2) t
						       `(eq ,arg1 ,arg2)))
		     (zctype>boolean)))))
	((and (or (zctype>pointer-p type1) (zctype>pointer-p type2))
	      (zctype>match type1 type2 env))
	 (values (zcprim>pointer-compare arg1 type1 arg2 type2 pred polarity)
		 (zctype>boolean)))
	((and (zctype>lispval-p type1) (zctype>lispval-p type2))
	 (when (neq pred 'eql)
	   (zcerror "lispvals cannot be compared with ~A" pred-name))
	 (values (zcprim>apply-polarity polarity `(eql ,arg1 ,arg2))
		 (zctype>boolean)))
	(t (zcerror "Argument types are incorrect or don't match to ~A: ~A and ~A"
		   pred-name type1 type2))))

; I'm not going to try to do constant folding in here.  Maybe later.
(defun zcprim>pointer-compare (arg1 type1 arg2 type2 pred polarity)
  (nlet ((let-clauses-1 array-1 index-1
			(zcprim>analyze-pointer-exp arg1 type1))
	 (let-clauses-2 array-2 index-2
			(zcprim>analyze-pointer-exp arg2 type2))
	 ((test-form
	    (cond ((neq pred 'eql)
		   `(progn ,(and (not *compare-incomparable-pointers*)
				 `(zcptr>compare-check ,array-1 ,array-2))
			   (,pred ,index-1 ,index-2)))
		  ((and (null array-1) (null array-2) (= index-1 index-2))
		   t)
		  ((null array-1) `(and (null ,array-2) (zerop ,index-2)))
		  ((null array-2) `(and (null ,array-1) (zerop ,index-1)))
		  (t `(and (eq ,array-1 ,array-2) (= ,index-1 ,index-2)))))))
    ;; These let clauses can't depend on each other, so we use APPEND.
    (zcprim>let-form (append let-clauses-1 let-clauses-2)
      (zcprim>apply-polarity polarity test-form))))

(defun zcprim>apply-polarity (polarity exp)
  "From boolean expression EXP, creates another expression which is T when EXP
   equals POLARITY."
  (cond (polarity exp)
	((memq exp '(T NIL)) (not exp))
	(t `(not ,exp))))

; These three can't get type errors!  Types checked in zcprim>standard-coercions.
(defprim c:!
  "Logical NOT."
  (((arg type :boolean))
   (values (if (memq arg '(T NIL)) (not arg) `(not ,arg))
	   type)))		   ; might be lispval

(defprim c:&&
  "Logical AND.  As you would expect, this only evaluates as many arguments
   as it needs to for a result (/"short-circuiting/")."
  ((&rest (args types :boolean))
   (values (if (every args #'(lambda (arg) (memq arg '(T NIL))))
	       (gmap (:and) :id (:list args))
	     `(and . ,args))
	   (if (gmap (:and) #'zctype>lispval-p (:list types)) (zctype>lispval)
	     (zctype>boolean)))))

(defprim c:/|/|
  "Logical OR.  As you would expect, this only evaluates as many arguments
   as it needs to for a result (/"short-circuiting/")."
  ((&rest (args types :boolean))
   (ignore types)
   (values (if (every args #'(lambda (arg) (memq arg '(T NIL))))
	       (gmap (:or) :id (:list args))
	     `(or . ,args))
	   (if (gmap (:and) #'zctype>lispval-p (:list types)) (zctype>lispval)
	     (zctype>boolean)))))


; ================================================================
; Assignment and modify-in-place operators.  C has a gaggle of these.

(defprim c:=
  "Plain old assignment."
  (((arg1 type1 :lvalue) (arg2 type2))
   (values (zcprim>store-value arg1 type1 arg2 type2 **env **context)
	   type1)))

(defun zcprim>store-value (dest dest-type src src-type env context)
  (let ((src (zcprim>coerce-numbers src src-type dest-type)))
    (cond ((not (zctype>match dest-type src-type env))
	   (zcwarn "Mismatched argument types to = (assignment): (~A)~A and (~A)~A; attempting cast"
		   (zclstn>type-string dest-type env) dest
		   (zclstn>type-string src-type env) src)
	   (zcprim>store-value dest dest-type
			       (zcprim>cast src src-type dest-type env) dest-type
			       env context))
	  ;; For C value printer only (arrays are not usually lvalues!)
	  ((zctype>array-p dest-type)
	   (zcprim>structure-assign src dest dest-type env))
	  ((zctype>arith-pointer-p dest-type)
	   (zcprim>store-pointer-value dest dest-type src src-type context))
	  ((zctype>struct-p dest-type)
	   (when (not *firstclass-structures*)
	     (zcerror "Assignments to structures are not allowed.  If you want to
  permit them (for Unix compatibility), set ZETA-C:*FIRSTCLASS-STRUCTURES* to T."))
	   (zcprim>structure-assign src dest dest-type env))
	  ;; If storing into a char or short variable that's not an array or
	  ;; packed-struct element, shorten explicitly.
	  ((zctype>shorter-than-int-p dest-type)
	   (nlet ((ignore ignore ignore arefs-p
		   (zcprim>analyze-aref-exp dest dest-type)))
	     `(zcprim>setf ,dest
			   ,(zcprim>shorten-if-necessary
			      src dest-type (and arefs-p (neq arefs-p 'aref))))))
	  ;; Ugly special case: assignment of NULL to ptr-to-func.  How generalize?
	  ((and (zctype>function-pointer-p dest-type) (zctype>zero-p src-type))
	   ;; We keep knowledge about the conversion in ZCPRIM>CAST.
	   ;; ZCPRIM>STANDARD-COERCIONS knows too.
	   `(zcprim>setf ,dest ,(zcprim>cast (zcprim>null-pointer)
					     (zctype>pointer-to (zctype>int))
					     dest-type env)))
	  (t `(zcprim>setf ,dest ,src)))))

(defun zcprim>store-pointer-value (dest dest-type src src-type context)
  (nlet ((src-lets array index
		   (zcprim>analyze-pointer-exp src src-type))
	 (dest-lets dest-array dest-index
		    (zcprim>analyze-pointer-exp dest dest-type))
	 ((array-temp array-val (zcprim>select-trivial array dest-array context)))
	 ((index-temp index-val (zcprim>select-trivial index dest-index context)))
	 ;; We have to use VALUES here to make sure both parts of the
	 ;; source are computed before either of the stores occur.
	 (((setf-lets `(ignore
			 (zcprim>setf (values ,dest-array ,dest-index)
				      (values ,(or array-temp array)
					      ,(or index-temp index))))))))
    (zcprim>let-form (zcprim>subordinate-nlet-clauses
		       (append src-lets dest-lets)
		       `(,@(and array-temp `((,array-temp ,array)))
			 ,@(and index-temp `((,index-temp ,index)))
			 ,(if (or array-temp index-temp) `(,setf-lets) setf-lets)))
      `(zcptr>cons ,array-val ,index-val))))

(defun zcprim>select-trivial (src dest context)
  (if (zcprim>trivial-p src) (values nil src)
    (if (zcprim>trivial-p dest) (values nil dest)
      (let ((temp (and (not (memq ':statement (car context)))
		   (zcprim>gen-var))))
	(values temp temp)))))

(defmacro zcprim>setf (dest value)
  "This is needed to handle a couple of cases that aren't handled naturally by
   Symbolics SETF."
  (cond ((nlistp dest) `(setq ,dest ,value))
	((eq (car dest) 'nlet)
	 `(nlet ,(cadr dest)
	    ,@(butlast (cddr dest))
	    (zcprim>setf ,(car (last (cddr dest))) ,value)))
	;; I can't believe this doesn't work already.
	((eq (car dest) 'progn)
	 `(progn ,@(butlast (cdr dest))
		 (zcprim>setf ,(car (last (cdr dest))) ,value)))
	;; The one case where we don't want to use the result; it would generate
	;; excessively poor code anyway.
	((eq (car dest) 'values)
	 (if (gmap (:and) #'symbolp (:list (cdr dest)))
	     `(multiple-value ,(cdr dest) ,value)
	   ;; This will not be the best code, because very-short-lifetime locals
	   ;; are poorly optimized.
	   (let ((temps (mapcar #'(lambda (ignore) (zcprim>gen-var))
				(cdr dest))))
	     `(nlet ((,@temps ,value))
		. ,(mapcar #'(lambda (place temp) `(zcprim>setf ,place ,temp))
			   (cdr dest) temps)))))
	(t `(#+Symbolics cl:setf #-Symbolics setf ,dest ,value))))

(defun zcprim>shorten-if-necessary (exp type override)
  "Generates an expression to shorten EXP to type TYPE, if that is one of the
   four short-integer types.  OVERRIDE, which should be true if the store is taking
   place into a byte array, prevents any shortening."
  (cond ((or override (not (zctype>shorter-than-int-p type))) exp)
	((zctype>char-p type) (zcprim>ldb '(byte 8 0) exp))
	((zctype>signed-short-p type) (zcprim>16-bit-sign-extend exp))
	((zctype>signed-char-p type) (zcprim>8-bit-sign-extend exp))
	((zctype>unsigned-short-p type) (zcprim>ldb '(byte 16 0) exp))
	(t (zcerror "Internal error: type ~A not accounted for!?" type))))

(defun zcprim>coerce-numbers (exp from-type to-type)
  "If necessary, wraps code around EXP to do numeric-type coercion."
  (cond ((and (not (zctype>double-p from-type))
	      (zctype>double-p to-type))
	 (zcprim>dfloat exp))
	((and (or (zctype>integer-p from-type) (zctype>double-p from-type))
	      (zctype>float-p to-type) (not (zctype>double-p to-type)))
	 (zcprim>float exp))
	((and (zctype>integer-p from-type) (zctype>float-p to-type))
	 (zcprim>float exp))
	((and (zctype>float-p from-type) (zctype>integer-p to-type))
	 ;; Note: C doesn't specify direction of truncation of negative numbers.
	 (zcprim>fix exp))
	(t exp)))

(defun zcprim>structure-assign (src dest type env)
  ;; I know this looks strange but it works iff zcprim>struct-reference does.
  (nlet ((int-array (zctype>array-of (zctype>int)))
	 ((src-lets src-array src-index (zcprim>analyze-pointer-exp src int-array))
	  (dst-lets dst-array dst-index
	    (zcprim>analyze-pointer-exp dest int-array))))
    (zcprim>let-form (zcprim>subordinate-nlet-clauses
		       (append src-lets dst-lets)
		       `((ignore (zcprim>copy-structure
				   ,src-array ,src-index ,dst-array ,dst-index
				   ,(zctype>sizeof-in-scale type env)))))
      `(zcptr>cons ,dst-array ,dst-index))))

#+Symbolics
(defun zcprim>copy-structure (src-array src-index dst-array dst-index length)
  (let ((src-array src-array) (dst-array dst-array))
    (declare (sys:array-register src-array dst-array))
    (dotimes (i length)
      (aset (aref src-array (+ i src-index)) dst-array (+ i dst-index)))))

#-Symbolics
(defun zcprim>copy-structure (src-array src-index dst-array dst-index length)
  ;; This assumes word alignment, but does not assume a particular scale.
  (nlet ((src-array (zcprim>array-as-q src-array))
	 (dst-array (zcprim>array-as-q dst-array))
	 (ascale (zcprim>array-scale src-array))
	 ((src-index src-byte
	    (floor (zcprim>rescale-index ascale :8B src-index) 4)))
	 ((dst-index dst-byte
	    (floor (zcprim>rescale-index ascale :8B dst-index) 4)))
	 ((length (ceiling (zcprim>rescale-index ascale :8B length) 4))))
    (when (or (not (zerop src-byte)) (not (zerop dst-byte)))
      (ferror "ZETA-C Internal error: src and dst must be word-aligned"))
    ;; First we copy the boxed data...
    (%blt-typed (aloc src-array src-index) (aloc dst-array dst-index) length 1)
    ;; ... and then the unboxed
    (let ((src-16b (zcprim>array-as-16b src-array))
	  (dst-16b (zcprim>array-as-16b dst-array))
	  (src-start (* 2 src-index))
	  (dst-start (* 2 dst-index))
	  (len (* 2 length)))
      (copy-array-portion src-16b src-start (+ src-start len)
			  dst-16b dst-start (+ dst-start len)))))

(defprim c:++x
  (((arg type :lvalue))
   (values (zcprim>pre-increment arg type "++ (pre)" #'zcprim>+
				 (zcprim>scale-ptr-offset 1 type **env)
				 (zctype>int))
	   (zctype>canonicalize-pointer type))))

(defprim c:x++
  (((arg type :lvalue))
   (values
     (if (memq ':statement (car **context))
	 ;; To avoid CONS D-IGNORE instructions.
	 (zcprim>pre-increment arg type "++ (post)" #'zcprim>+
			       (zcprim>scale-ptr-offset 1 type **env) (zctype>int))
       (zcprim>post-increment arg type "++ (post)" #'zcprim>+
			      (zcprim>scale-ptr-offset 1 type **env) (zctype>int)))
     (zctype>canonicalize-pointer type))))

(defprim c:--x
  (((arg type :lvalue))
   (values (zcprim>pre-increment arg type "-- (pre)" #'zcprim>+
				 (zcprim>scale-ptr-offset -1 type **env)
				 (zctype>int))
	   (zctype>canonicalize-pointer type))))

(defprim c:x--
  (((arg type :lvalue))
   (values
     (if (memq ':statement (car **context))
	 ;; To avoid CONS D-IGNORE instructions.
	 (zcprim>pre-increment arg type "-- (post)" #'zcprim>+
			       (zcprim>scale-ptr-offset -1 type **env)
			       (zctype>int))
       (zcprim>post-increment arg type "-- (post)" #'zcprim>+
			      (zcprim>scale-ptr-offset -1 type **env)
			      (zctype>int)))
     (zctype>canonicalize-pointer type))))

(defun zcprim>increment (arg type name symbol-fun aref-exp-fun bit-field-exp-fun)
  (cond ((zctype>arith-pointer-p type)
	 (nlet ((let-clauses array index (zcprim>analyze-pointer-exp arg type))
		((index++ (zcprim>increment index (zctype>int) name
					    symbol-fun aref-exp-fun nil))))
	   (zcprim>let-form let-clauses `(zcptr>cons ,array ,index++))))
	((zctype>number-p type)
	 (bcond ((symbolp arg)
		 (funcall symbol-fun arg))
		(((let-clauses array index arefs-p
		   (if (memq (car arg) '(ldb ldb-signed))
		       (zcprim>analyze-aref-exp (caddr arg) type)
		     (zcprim>analyze-aref-exp arg type)))
		  arefs-p)
		 ;; We assume that the array and index are both referenced twice.
		 (nlet ((array ar-lets (zcprim>make-temp-if-needed array))
			(index idx-lets (zcprim>make-temp-if-needed index)))
		   (zcprim>let-form (zcprim>subordinate-nlet-clauses
				      let-clauses (append ar-lets idx-lets))
		     (if (memq (car arg) '(ldb ldb-signed))
			 (funcall bit-field-exp-fun array index
						    (car arg) (cadr arg))
		       (funcall aref-exp-fun arefs-p array index)))))
		(t (zcerror "Internal error: Can't apply /"~A/" to ~A" name arg))))
	(t (zcerror "Wrong argument type to ~A: ~A" name type))))

(defun zcprim>pre-increment (arg type name op arg2 type2)
  (zcprim>increment
    arg type name
    #'(lambda (sym)
	(nlet ((arg1 ignore arg2 type2
		     (zcprim>standard-binary-coercions sym type arg2 type2)))
	  `(setq ,sym ,(zcprim>shorten-if-necessary
			 (zcprim>coerce-numbers (funcall op arg1 arg2) type2 type)
			 type nil))))
    #'(lambda (aref-sym array index)
	(nlet ((arg1 ignore arg2 type2
		     (zcprim>standard-binary-coercions `(,aref-sym ,array ,index)
						       type arg2 type2)))
	  `(,(zcprim>aref-to-aset aref-sym)
	    ,(zcprim>shorten-if-necessary
	       (zcprim>coerce-numbers (funcall op arg1 arg2) type2 type)
	       type (neq aref-sym 'aref))
	    ,array ,index)))
    ;; Yeesh.  Bit fields.  What a mess.  Oh well.
    #'(lambda (array index ldb-sym byte)
	(nlet ((word-var (zcprim>gen-var))
	       (byte-var (zcprim>gen-var))
	       (arg1 ignore arg2 type2
		     (zcprim>standard-binary-coercions `(aref ,array ,index)
						       type arg2 type2)))
	  `(nlet ((,word-var ,arg1)
		  ((,byte-var ,(zcprim>coerce-numbers
				 (funcall op `(,ldb-sym ,byte ,word-var) arg2)
				 type2 type))))
	     (aset (dpb ,byte-var ,byte ,word-var) ,array ,index)
	     ,byte-var)))))

(defun zcprim>post-increment (arg type name op arg2 type2)
  (zcprim>increment
    arg type name
    #'(lambda (sym)
	(nlet ((arg1 ignore arg2 type2
		     (zcprim>standard-binary-coercions sym type arg2 type2)))
	  `(prog1 ,sym (setq ,sym ,(zcprim>shorten-if-necessary
				     (zcprim>coerce-numbers (funcall op arg1 arg2)
							    type2 type)
				     type nil)))))
    #'(lambda (aref-sym array index)
	(nlet ((temp-var (zcprim>gen-var))
	       (arg1 ignore arg2 type2
		     (zcprim>standard-binary-coercions `(,aref-sym ,array ,index)
						       type arg2 type2)))
	  `(nlet ((,temp-var ,arg1))
	     (progn (,(zcprim>aref-to-aset aref-sym)
		     ,(zcprim>shorten-if-necessary
			(zcprim>coerce-numbers (funcall op temp-var arg2)
					       type2 type)
			type (neq aref-sym 'aref))
		     ,array ,index)
		    ,temp-var))))
    #'(lambda (array index ldb-sym byte)
	(nlet ((word-var (zcprim>gen-var))
	       (byte-var (zcprim>gen-var))
	       (arg1 ignore arg2 type2
		     (zcprim>standard-binary-coercions `(aref ,array ,index) type
						       arg2 type2)))
	  `(nlet ((,word-var ,arg1)
		  ((,byte-var (,ldb-sym ,byte ,word-var))))
	     (aset (dpb ,(zcprim>coerce-numbers (funcall op byte-var arg2)
						type2 type)
			,byte ,word-var)
		   ,array ,index)
	     ,byte-var)))))

(defprim c:+=
  (((arg1 type1 :lvalue) (arg2 type2))
   (zcprim>op-assign arg1 type1 arg2 type2 **env "+=" t t 'zcprim>+)))

(defprim c:-=
  (((arg1 type1 :lvalue) (arg2 type2))
   (zcprim>op-assign arg1 type1 arg2 type2 **env "-=" t t 'zcprim>-)))

(defprim c:*=
  (((arg1 type1 :lvalue) (arg2 type2))
   (zcprim>op-assign arg1 type1 arg2 type2 **env "*=" nil t 'zcprim>*)))

(defprim c://=
  (((arg1 type1 :lvalue) (arg2 type2))
   (zcprim>op-assign arg1 type1 arg2 type2 **env "//=" nil t 'zcprim>//)))

(defprim c:%=
  (((arg1 type1 :lvalue) (arg2 type2))
   (zcprim>op-assign arg1 type1 arg2 type2 **env "%=" nil t 'zcprim>\)))

(defprim c:<<=
  (((arg1 type1 :lvalue) (arg2 type2))
   (zcprim>op-assign arg1 type1 arg2 type2 **env "<<=" nil nil 'zcprim>ash)))

(defprim c:>>=
  (((arg1 type1 :lvalue) (arg2 type2))
   (zcprim>op-assign arg1 type1 (zcprim>- arg2) type2 **env ">>=" nil nil
		     'zcprim>ash)))

(defprim c:&=
  (((arg1 type1 :lvalue) (arg2 type2))
   (zcprim>op-assign arg1 type1 arg2 type2 **env "&=" nil nil 'zcprim>logand)))

(defprim c:/|=
  (((arg1 type1 :lvalue) (arg2 type2))
   (zcprim>op-assign arg1 type1 arg2 type2 **env "|=" nil nil 'zcprim>logior)))

(defprim c:^=
  (((arg1 type1 :lvalue) (arg2 type2))
   (zcprim>op-assign arg1 type1 arg2 type2 **env "^=" nil nil 'zcprim>logxor)))

(defun zcprim>op-assign (arg1 type1 arg2 type2 env name pointer-allowed-p
			 float-allowed-p operation)
  (if (or (and (or (and float-allowed-p (zctype>number-p type1))
		   (zctype>integer-p type1))
	       (or (and float-allowed-p (zctype>number-p type2))
		   (zctype>integer-p type2)))
	  (and pointer-allowed-p
	       (zctype>arith-pointer-p type1) (zctype>integer-p type2)))
      (values (zcprim>pre-increment arg1 type1 name operation
				    (zcprim>scale-ptr-offset arg2 type1 env) type2)
	      (zctype>canonicalize-pointer type1))
    (zcerror "Wrong argument type to ~A: ~A and//or ~A" name type1 type2)))


; ================================================================
; Function calling.

(defun zcprim>trans-misc-fun-call (exp env context)
  "Translates a call to a user function from C into Lisp, returning two values: the
   translated expression and its type."
  (nlet ((fun-exp fun-type
		  (zcmac>translate-exp (car exp) env (cons '(:funcall) context))))
    (zcprim>trans-misc-fun-call-1 fun-exp fun-type exp env context)))
(defun zcprim>trans-misc-fun-call-1 (fun-exp fun-type exp env context)
  (nlet ((trans-exp ret-type (zcprim>trans-misc-fun-call-2 fun-exp fun-type
							   exp env context)))
    (if (or (zctype>arith-pointer-p ret-type)
	    ;; Structures look like pointers thereto.
	    (zctype>struct-p ret-type))
	(let ((array-temp (zcprim>gen-var))
	      (index-temp (zcprim>gen-var)))
	  (values (zcprim>let-form `((,array-temp ,index-temp ,trans-exp))
		    `(zcptr>cons ,array-temp ,index-temp))
		  ret-type))
      (values trans-exp ret-type))))
(defun zcprim>trans-misc-fun-call-2 (fun-exp fun-type exp env context)
  (nlet ((arg-exps arg-types let-clauses
		   (zcprim>trans-function-arg-list (cdr exp) env context)))
    (cond ((zctype>function-pointer-p fun-type)
	   ;; Be mellow about calling function pointers, per ANSI draft.
	   (values (zcprim>let-form let-clauses `(funcall ,fun-exp . ,arg-exps))
		   (zctype>function-return-type
		     (zctype>pointer-deref-type fun-type))))
	  ((not (zctype>function-p fun-type))
	   (zcerror "Attempt to call ~A of type ~A as a function"
		    (car exp) fun-type))
	  ((listp (car exp))		; Explicit func ptr deref (e.g., "(*fp)()")
	   (values (zcprim>let-form let-clauses `(funcall ,fun-exp . ,arg-exps))
		   (zctype>function-return-type fun-type)))
	  (t
	   (zcprim>caller-check-arg-types fun-exp fun-type arg-types
					  (zcprim>function-name context))
	   (values (zcprim>let-form let-clauses `(,fun-exp . ,arg-exps))
		   (zctype>function-return-type fun-type))))))

(defun zcprim>trans-function-arg-list (raw-args env context)
  "Translates the argument expressions RAW-ARGS into the argument forms that will
   actually be passed to the function.  Returns three values: a list of argument
   forms, a list of types, and a list of let-clauses for the function-call form.
   Note that pointer arguments turn into two forms, so there can be more forms than
   types."
  (nlet ((arg-exps arg-types
		   (zcmac>translate-rest-arg raw-args t env (cons nil context))))
    (zcprim>trans-function-arg-list-1 arg-exps arg-types)))
(defun zcprim>trans-function-arg-list-1 (arg-exps arg-types)
  (if (null arg-exps)
      (values nil nil nil)
    (nlet ((arg type (zcprim>standard-unary-coercions (car arg-exps)
						      (car arg-types)))
	   (cdr-args cdr-types cdr-let-clauses
	    (zcprim>trans-function-arg-list-1 (cdr arg-exps) (cdr arg-types))))
      (if (not (zctype>arith-pointer-p type))
	  (values (cons arg cdr-args) (cons type cdr-types) cdr-let-clauses)
	(nlet ((let-clauses array index (zcprim>analyze-pointer-exp arg type)))
	  (values (cons array (cons index cdr-args))
		  (cons (zctype>canonicalize-pointer type) cdr-types)
		  (append let-clauses cdr-let-clauses)))))))

; This one is used when a function is called...
(defun zcprim>caller-check-arg-types (fun fun-type arg-types caller)
  "Checks the function's type and argument types against the declared types, if
   known.  Also accumulates them for future checking."
  (nlet ((zcenv>*substitute-quietly* t)	     ; for ZCENV>DEFINITION call below.
	 ((fun-type (if (zctype>function-param-types-p fun-type)
			fun-type	     ; The locally visible type may not
		      (and (symbolp fun)     ; have param types; look around.
			   (zcenv>definition fun (zcenv>global-env)))))))
    (when (and fun-type (zctype>function-param-types-p fun-type))
      (zctype>arglist-type-check arg-types (zctype>function-param-types fun-type)
				 fun caller)))
  (when (and caller (symbolp fun))
    (zcprim>accum-call-info `(,caller ,fun ,(zctype>function-return-type fun-type)
			      . ,arg-types))))

(defun zcprim>accum-call-info (call)
  "Adds the information about this call to the list for the function being
   compiled; but only if it's nonredundant."
  (when (not (gmap (:or) #'(lambda (ocall)
			     (and (eq (second call) (second ocall))
				  (= (length call) (length ocall))
				  (gmap (:and) #'(lambda (type otype)
						   (and (zctype>equal type otype)))
					(:list (secondcdr call))
					(:list (secondcdr ocall)))))
		   (:list zcprim>*defun-function-call-info*)))
    (push call zcprim>*defun-function-call-info*)))

; ... and this one when it's defined (actually, at load time).
(defun zcprim>load-check-arg-types (fun *source-location*)
  "Checks the recorded call info for this function (who calls it with which types,
   and what type was it declared to return) against the correct types."
  (nlet ((fun-type (zcenv>function-type fun (zcenv>global-env)))
	 ((return-type (zctype>function-return-type fun-type))
	  (param-types (zctype>function-param-types fun-type)))
	 (zcprim>*expanding-defunc+* fun))
    (dolist (call-info-entry (get fun 'call-info-alist))
      (let ((caller (car call-info-entry))
	    (calls (cdr call-info-entry)))
	(dolist (call calls)
	  (zctype>arglist-type-check (thirdcdr call) param-types fun caller)
	  (when (and (not (zctype>equal return-type (third call)))
		     (not (zctype>void-p (third call))))
	    (zcwarn "Function ~A assumed by ~A to return type /"~A/"; actual type /"~A/""
		    fun caller (zclstn>type-string (third call))
		    (zclstn>type-string return-type))))))))

; Finally, this is used at load time to tell the world about our callees.
(defun zcprim>record-call-info (call-info)
  "Records the accumulated call info for this function's callees.  I.e., for each
   callee, maintains an alist (on its CALL-INFO-ALIST property) of the form
   ((<caller> <call-info> <call-info> ...) ...).  Each <call-info> is of the form
   (<caller> <callee> <return-type> <param-type> <param-type> ...)."
  ;; We sort prior to grouping.  Sort predicate hardly matters.
  (nlet ((sorted (sort call-info
		       #'(lambda (i1 i2) (alphalessp (second i1) (second i2)))))
	 ((grouped (group sorted #'(lambda (ci1 ci2)
				     (eq (second ci1) (second ci2)))))))
    (dolist (group grouped)
      (nlet ((callee (second (car group)))
	     ((alist (get callee 'call-info-alist))
	      ((frame (assq (first (car group)) alist)))))
	(if (null frame)
	    (push (cons (first (car group)) group) (get callee 'call-info-alist))
	  (rplacd frame group))))))


; ================================================================
; Array and structure referencing.

(defprim c:[]
  (((arg1 type1) (arg2 type2))
   (cond ((and (zctype>arith-pointer-p type1) (zctype>integer-p type2))
	  (nlet ((offset-exp type
		  (zcprim>pointer-plus-int arg1 arg2 type1 #'zcprim>+ **env)))
	    (zcprim>deref-pointer offset-exp type **env)))
	 ((and (zctype>arith-pointer-p type2) (zctype>integer-p type1))
	  (nlet ((offset-exp type
		  (zcprim>pointer-plus-int arg2 arg1 type2 #'zcprim>+ **env)))
	    (zcprim>deref-pointer offset-exp type **env)))
	 (t
	  (zcerror "Wrong argument type to [] (array ref): ~A or ~A"
		   type1 type2)))))

(defprim c:/.
  (((struct stype) &quote elt)
   (zcprim>struct-reference "." struct stype stype elt **env **context)))

(defprim c:->
  (((struct-p stype) &quote elt)
   (if (not (zctype>pointer-p stype))
       (zcerror "Wrong first argument type to -> (struct//union reference): ~A"
		stype)
     (zcprim>struct-reference "->" (zcprim>deref-pointer struct-p stype **env)
			      (zctype>pointer-deref-type stype)
			      stype elt **env **context))))

(defun zcprim>struct-reference (name struct stype origtype elt env context)
  "Process a structure (or union) reference."
  (ignore context)
  (if (not (zctype>struct-p stype))
      ;; Extension to allow arbitrary kinds of "struct" references.
      (nlet ((hook (and (zctype>lispval-p stype) (zctype>get-lispval-hook stype))))
	(if hook (funcall hook :struct-reference stype struct elt env)
	  (zcerror "Wrong first argument type to ~A (struct//union reference): ~A"
		   name origtype)))
    (nlet ((elts (zctype>struct-elements stype env))
	   ((eltpr (assq elt elts))
	    ((elt-type (zctype>eltpr-type eltpr)))))
      (if (not eltpr)
	  (zcerror "Element ~A not found in struct//union type ~A" elt stype)
	(nlet ((access-exp result-type
		(zcprim>deref-pointer
		  (zcprim>pointer-plus-int
		    (zcprim>coerce-struct-pointer struct elt-type
						  (neq (zctype>struct-class stype)
						       :packed-struct))
		    (selectq (zctype>struct-class stype)
		      ((:struct :packed-struct)
		       (zctype>struct-elt-offset elt stype env))
		      (:union 0))
		    (zctype>array-of (zctype>int)) #'zcprim>+ env)
		  (zctype>pointer-to elt-type) env
		  (neq (zctype>struct-class stype) :packed-struct))))
	  (zcprim>struct-hack-bit-field access-exp elt-type result-type))))))

(defun zcprim>struct-hack-bit-field (access-exp elt-type result-type)
  "Given an expression to access a struct element, if the element is a bit field
   within that word, wraps an appropriate LDB around the expression."
  (if (not (zctype>bits-p elt-type)) (values access-exp result-type)
    (let ((ldb-form (if (zctype>unsigned-p (cadr elt-type)) 'ldb 'ldb-signed)))
      (values `(,ldb-form (byte ,(caddr elt-type) ,(cadddr elt-type)) ,access-exp)
	      (if (zctype>unsigned-p (cadr elt-type))
		  (zctype>unsigned) (zctype>int))))))

(defmacro ldb-signed (bytespec word)
  (unless (and (listp bytespec) (and (eq (car bytespec) 'byte)))
    (ferror "ZETA-C internal error: (BYTE ...) form expected, not ~S" bytespec))
  (nlet ((temp (zcprim>gen-var))
	 (size (cadr bytespec)))
    `(nlet ((,temp (ldb ,bytespec ,word)))
       (dpb ,temp (byte ,(1- size) 0) (- (ldb (byte 1 ,(1- size)) ,temp))))))
#+Symbolics (putprop 'ldb-signed (get 'ldb 'lt::setf-method) 'lt::setf-method)
#-Symbolics (putprop 'ldb-signed (get 'ldb 'si:setf-method) 'si:setf-method)

(defun zcprim>coerce-struct-pointer (ptr elt-type unpacked)
  "Given a pointer to some kind of struct, coerces it to point to ELT-TYPE."
  (nlet ((let-clauses array index
	  (zcprim>analyze-pointer-exp ptr (zctype>array-of (zctype>int)))))
    (selectq (zctype>type-scale elt-type unpacked)
      (:Q ptr)
      (:16B (zcprim>let-form let-clauses
	      `(zcptr>cons (zcprim>art-16b-slot ,array) ,(zcprim>* index 2))))
      (:8B (zcprim>let-form let-clauses
	     `(zcptr>cons (zcprim>art-8b-slot ,array) ,(zcprim>* index 4)))))))

(defun zcprim>array-leader-init (name type env)
  `((,name ,type ,env) nil nil nil nil nil #+Chars nil))

(defun zcprim>cast-array-leader-init (parent)
  `(,parent nil nil nil nil nil #+Chars nil))

(defsubst zcprim>array-leader-length ()
  #-Chars 6 #+Chars 7)

(defsubst zcprim>array-desc (frob)
  (array-leader frob 0))

;;; In general, use NAMED-STRUCTURE-P instead.
(defsubst zcprim>array-named-structure-symbol (frob)
  (array-leader frob 1))

(defsubst zcprim>array-freelist-link (frob)
  (array-leader frob 2))

(defsubst zcprim>art-q-slot (frob)
  (array-leader frob 3))

(defsubst zcprim>art-16b-slot (frob)
  (array-leader frob 4))

(defsubst zcprim>art-8b-slot (frob)
  (array-leader frob 5))

#+Chars
(defsubst zcprim>art-string-slot (frob)
  (array-leader frob 6))

(defun zcprim>array-consed-type (frob)
  "Returns the type with which FROB was originally consed."
  (let ((desc (zcprim>array-desc frob)))
    (if (arrayp desc)
	(cadr (zcprim>array-desc desc))
      (cadr desc))))

(defun zcprim>array-consed-env (frob)
  "Returns the type with which FROB was originally consed."
  (let ((desc (zcprim>array-desc frob)))
    (if (arrayp desc)
	(caddr (zcprim>array-desc desc))
      (caddr desc))))

(defun zcprim>scale-slot (scale frob)
  "Reads the slot of FROB corresponding to SCALE (one of :Q, :16B, or :8B)."
  (selectq scale
    (:Q (zcprim>art-q-slot frob))
    (:16B (zcprim>art-16b-slot frob))
    (:8B (zcprim>art-8B-slot frob))
    #+Chars (:STRING (zcprim>art-string-slot frob))))

(defun zcprim>store-scale-slot (scale frob value)
  "Stores VALUE into the slot of FROB corresponding to SCALE (one of :Q, :16B, or
   :8B)."
  (selectq scale
    (:Q (setf (zcprim>art-q-slot frob) value))
    (:16B (setf (zcprim>art-16b-slot frob) value))
    (:8B (setf (zcprim>art-8B-slot frob) value))
    #+Chars (:STRING (setf (zcprim>art-string-slot frob) value))))

#+Symbolics 
(defprop zcprim>scale-slot
	 ((zcprim>scale-slot scale frob)
	  . (zcprim>store-scale-slot scale frob si:val))
	 setf)

#-Symbolics 
(defsetf zcprim>scale-slot zcprim>store-scale-slot)

(defun zcprim>rescale-array (from-scale to-scale frob)
  "Makes a form which returns a version of FROB corresponding to SCALE (one of :Q,
   :16B, or :8B), using the cached one or creating and caching one as appropriate."
  (if (eq from-scale to-scale) frob
    (selectq to-scale
      (:Q `(zcprim>array-as-q ,frob))
      (:16B `(zcprim>array-as-16b ,frob))
      (:8B `(zcprim>array-as-8b ,frob))
      #+Chars (:STRING `(zcprim>array-as-string ,frob)))))

(defun zcprim>rescale-index (from-scale to-scale index)
  (zcprim>ash index
	      (- (haulong (zctype>scale-size from-scale))
		 (haulong (zctype>scale-size to-scale)))))

(defun zcprim>link-slot-web (primary)
  "Fills in the web of connections that link the various versions of PRIMARY
   together."
  (dolist (sscale '(:Q :16B :8B #+Chars :STRING))
    (let ((sslot (zcprim>scale-slot sscale primary)))
      (and sslot
	   (dolist (dscale '(:Q :16B :8B #+Chars :STRING))
	     (let ((dslot (zcprim>scale-slot dscale primary)))
	       (and dslot (zcprim>store-scale-slot
			    dscale sslot
			    (follow-structure-forwarding dslot)))))))))

(defun zcprim>fill-scale-slots (frob)
  "Ensures that all scale versions of FROB exist.  Returns FROB."
  (zcprim>array-as-q frob)
  (zcprim>array-as-16b frob)
  (zcprim>array-as-8b frob)
  frob)

(defsubst zcprim>array-scale (frob)
  (zcprim>art-scale (array-type frob)))

(defun zcprim>array-as-scale (scale frob)
  "Returns a version of FROB corresponding to SCALE (one of :Q, :16B, or :8B),
   using the cached one or creating and caching one as appropriate."
  (selectq scale
    (:Q (zcprim>array-as-q frob))
    (:16B (zcprim>array-as-16b frob))
    (:8B (zcprim>array-as-8b frob))))

(defun zcprim>array-primary (frob)
  "The primary (originally allocated) view of FROB."
  (and frob				; NIL protection.
       (if (neq (named-structure-p frob) 'cast-array) frob
	 (array-leader frob 0))))

(defun zcprim>array-boxed-actual (frob)
  "Returns the array that actually holds the boxed storage of FROB, as opposed
   to being displaced onto that storage.  (On the 3600, of course, there is no
   boxed/unboxed distinction.)"
  (and frob				; NIL protection.
       #+3600 (zcprim>array-primary frob)
       #-3600 (zcprim>art-q-slot frob)))

(defun zcprim>array-unboxed-actual (frob)
  "Returns the array that actually holds the unboxed storage of FROB, as opposed
   to being displaced onto that storage.  (On the 3600, of course, there is no
   boxed/unboxed distinction.)"
  (and frob				; NIL protection.
       (let ((frob (zcprim>array-primary frob)))
	 #+3600 frob
	 #-3600 (if (neq (array-type frob) 'art-q) frob
		  (let ((8b (zcprim>art-8b-slot frob)))
		    (if (and 8b (not (array-displaced-p 8b))) 8b
		      (zcprim>art-16b-slot frob)))))))

(defun zcprim>array-as-q (frob)
  #+3600
  "Returns an ART-Q array displaced onto FROB, using the cached one or creating and
   caching a new one as appropriate."
  #-3600
  "Returns the ART-Q part of FROB, using the cached one or creating and caching a
   new one as appropriate.  Note that the ART-Q part does not share storage with
   the ART-16B and ART-8B parts."
  (if (null frob) nil			     ; For null pointer.
    (zcprim>check-array-leader frob)
    (or (zcprim>art-q-slot frob)
	(nlet ((elts//word (// 4 (zctype>scale-size (zcprim>array-scale frob))))
	       (length (array-length frob))
	       (actual (zcprim>array-boxed-actual frob))
	       ((q-frob (make-array
			  (ceiling length elts//word)
			  #+3600 :displaced-to #+3600 actual
			  :named-structure-symbol 'cast-array
			  :leader-list (zcprim>cast-array-leader-init
					 (zcprim>array-primary frob))))))
	  (setf (zcprim>art-q-slot frob) q-frob)
	  ;; The following allows us to access the last word in the Q version, even
	  ;; though not all of that word is contained in the byte array.
	  #+3600
	  (progn ;; "(max 1 ...)" keeps ALOC from erroring on 0-length frob.
		 (adjust-array-size frob (upto-next (max 1 length) elts//word))
		 (setf (si:array-indirect-pointer q-frob) (aloc q-frob 0))
		 (adjust-array-size frob length))
	  (zcprim>link-slot-web frob)
	  q-frob))))

(defun zcprim>array-as-16b (frob)
  #+3600
  "Returns an ART-16B array displaced onto FROB, using the cached one or creating
   and caching a new one as appropriate."
  #-3600
  "Returns an ART-16B array displaced onto the unboxed part of FROB, using the
   cached one or creating and caching a new one as appropriate."
  (if (null frob) nil			     ; For null pointer.
    (zcprim>check-array-leader frob)
    (or (zcprim>art-16b-slot frob)
	(nlet ((elts//word (// 4 (zctype>scale-size (zcprim>array-scale frob))))
	       (length (array-length frob))
	       (actual (zcprim>array-unboxed-actual frob))
	       ((16b-frob (make-array
			    (ceiling (* length 2) elts//word)
			    :type art-16b :named-structure-symbol 'cast-array
			    :displaced-to actual
			    :leader-list (zcprim>cast-array-leader-init
					   (zcprim>array-primary frob))))))
	  (setf (zcprim>art-16b-slot frob) 16b-frob)
	  (zcprim>link-slot-web frob)
	  16b-frob))))

(defun zcprim>array-as-8b (frob)
  #+3600
  "Returns an ART-8B array displaced onto FROB, using the cached one or creating
   and caching a new one as appropriate."
  #-3600
  "Returns an ART-16B array displaced onto the unboxed part of FROB, using the
   cached one or creating and caching a new one as appropriate."
  (if (null frob) nil			     ; For null pointer.
    (zcprim>check-array-leader frob)
    (or (zcprim>art-8b-slot frob)
	(let ((8b-frob (zcprim>array-as-bytes frob art-8b)))
	  (setf (zcprim>art-8b-slot frob) 8b-frob)
	  (zcprim>link-slot-web frob)
	  8b-frob))))

(defun zcprim>array-as-bytes (frob art)
  "Common portion of ZCPRIM>ARRAY-AS-8B and ZCPRIM>ARRAY-AS-STRING."
  (let ((scale-size (zctype>scale-size (zcprim>array-scale frob)))
	(length (array-length frob))
	(actual (zcprim>array-unboxed-actual frob)))
    (make-array (* length scale-size)
		:type art :named-structure-symbol 'cast-array
		:displaced-to actual
		:leader-list (zcprim>cast-array-leader-init
			       (zcprim>array-primary frob)))))

;;; This is used by the I/O library so we can talk to the outside world in terms
;;; of characters.
#+Chars
(defun zcprim>array-as-string (frob)
  #+3600
  "Returns an ART-STRING array displaced onto FROB, using the cached one or
   creating and caching a new one as appropriate."
  #-3600
  "Returns an ART-STRING array displaced onto the unboxed part of FROB, using the
   cached one or creating and caching a new one as appropriate."
  (if (null frob) nil			; For null pointer.
    (zcprim>check-array-leader frob)
    (or (zcprim>art-string-slot frob)
	(let ((string-frob (zcprim>array-as-bytes frob art-string)))
	  (setf (zcprim>art-string-slot frob) string-frob)
	  (zcprim>link-slot-web frob)
	  string-frob))))

(defun zcprim>check-array-leader (frob)
  "Verifies that this array has a sufficiently long leader for it to be a ZETA-C
   object; if not, makes a new version of it (with elements of the new leader
   all NIL) and forwards the old one to the new one.  This is needed for casting
   two different kinds of things: strings, and objects created by Lisp."
  (when (and (neq (named-structure-p frob) 'extern-dummy)
	     (or (not (fixp (array-leader-length frob)))
		 (< (array-leader-length frob) (zcprim>array-leader-length))))
    (let ((frob frob)			     ; for array register.
	  (new (make-array (array-active-length frob) :type (array-type frob)
			   :named-structure-symbol 'array
			   :leader-list
			   (zcprim>array-leader-init
			     nil
			     (zctype>array-of (zctype>scale-type
						(zcprim>array-scale frob))
					      (array-active-length frob))
			     (zcenv>global-env)))))
      #+Symbolics (declare (sys:array-register frob new))
      (dotimes (i (array-active-length frob))
	(setf (aref new i) (aref frob i)))
      (zcprim>store-scale-slot (zcprim>array-scale new) new new)
      (structure-forward frob new))))

(defun zcprim>art-scale (art)
  (selectq art
    (art-q :Q)
    (art-16b :16B)
    ((art-8b art-string) :8B)))

(defun zcprim>scale-art (scale)
  "Returns the ART- symbol for the specified scale."
  (selectq scale
    (:Q 'art-q)
    (:16B 'art-16b)
    (:8B 'art-8b)))


; ================================================================
; Control structure.

; These two are expressions.

(defprim c:progn+
  "The C /"comma/" operator."
  ((&rest (args types))
   (values (zcprim>let-form
	     ;; Statementize all ignored exps (prevents CONS D-IGNORE).
	     ;; Also, we use a let-form so the last expression can be found by
	     ;; the pointer analyzer.
	     `((ignore
		 (progn . ,(mapcan #'zcprim>exp-to-statement
				   (butlast args) (butlast types)))))
	     (car (last args)))
	   (car (last types)))))

(defprim c:?/:
  (((pred ptype :boolean) (cons ctype :binary) (alt atype :binary))
   (ignore ptype)			   ; Handled by zcprim>standard-coercions.
   (cond ((not (zctype>match ctype atype **env))
	  (zcerror "Mismatched consequent and alternate types to /"?:/": ~A and ~A"
		   ctype atype))
	 ((memq pred '(T NIL))	   ; constant fold.
	  (if pred (values cons ctype) (values alt atype)))
	 ((zctype>arith-pointer-p ctype)
	  (nlet ((c-lets c-array c-index (zcprim>analyze-pointer-exp cons ctype))
		 (a-lets a-array a-index (zcprim>analyze-pointer-exp alt atype))
		 (res-array (zcprim>gen-var))
		 (res-index (zcprim>gen-var)))
	    ;; Damn I'm brilliant.  What a pain this is!
	    (values (zcprim>let-form
		      `((,res-array ,res-index
			 (if ,pred ,(zcprim>let-form c-lets
				      `(values ,c-array ,c-index))
			   ,(zcprim>let-form a-lets `(values ,a-array ,a-index)))))
		      `(zcptr>cons ,res-array ,res-index))
		    ;; If one of these is a NULL, must canonicalize the OTHER one.
		    (if (or (zctype>zero-p ctype) (zctype>null-pointer-p ctype))
			(zctype>canonicalize-pointer atype)
		      (zctype>canonicalize-pointer ctype)))))
	 (t (values `(if ,pred ,cons ,alt) ctype)))))

; The rest of these are statements.  A statement produces a list of top-level
; forms.

(defprim c:|if|
  (((pred ptype :boolean) (cons ctype :statement) &optional (alt atype :statement))
   (ignore ptype ctype atype)	      ; ptype handled by zcprim>standard-coercions.
   (if alt
       (let ((alt-label (zcprim>gen-label))
	     (next-label (zcprim>gen-label)))
	 `((if (not ,pred) (go ,alt-label))
	   ,@cons
	   (go ,next-label)
	   ,alt-label
	   ,@alt
	   ,next-label))
     (let ((next-label (zcprim>gen-label)))
       `((if (not ,pred) (go ,next-label))
	 ,@cons
	 ,next-label)))))

(defprim c:block+
  ((&quote decls &body body)
   (nlet ((env (zcenv>create-env **env))
	  ((auto-inits specials static-inits toplevel-forms
		       (zcdecl>local-declarations decls env))
	   ((init-forms (zcprim>auto-init-forms auto-inits env
						(cons '(:statement) **context))))
	   ((freelist-clauses (zcprim>auto-freelist-clauses auto-inits)))))
     (setq zcprim>*defun-specials* (nconc specials zcprim>*defun-specials*))
     (setq zcprim>*defun-static-inits*
	   (nconc static-inits zcprim>*defun-static-inits*))
     (setq zcprim>*defun-toplevel-forms*
	   (nconc zcprim>*defun-toplevel-forms* toplevel-forms))
     (nlet ((d-block-frame (and auto-inits (list ':d-block (ncons ':setjmps))))
	    ((block-ctx (list* d-block-frame `(:locals . ,auto-inits) **context))
	     ((body (append init-forms
			    (zcprim>translate-block body env block-ctx)))
	      (cleanups (zcprim>auto-freelist-cleanups freelist-clauses env))
	      ;; Now that we've translated the body, we know what locals will need
	      ;; address arrays.
	      ((let-clauses progvars cleanups addl-inits
			    (zcprim>setup-locals auto-inits env cleanups))
	       ((body (zcprim>setup-setjmps (append addl-inits body)
					    (cdr (cadr d-block-frame))))
		((body (if (null cleanups) body
			 `((unwind-protect (prog () . ,body) . ,cleanups))))
		 ((body (if (null progvars) body
			  `((prog ,progvars
				  (declare (unspecial . ,progvars))
			       . ,body)))))))))))
       (if (null let-clauses) body
	 `((local-declare ((unspecial . ,(gmap:nlet>bound-vars let-clauses)))
	     (nlet ,let-clauses
	       . ,(if (and (listp (car body)) (eq (caar body) 'prog)) body
		    `((prog () . ,body)))))))))))

(defun zcprim>auto-init-forms (init-clauses env context)
  "Given a list of clauses of the form (<var> <type> <init>), returns a list of
   Lisp forms to assign the initial values to the variables."
  (mapcan #'(lambda (clause)
	      ;; The init-exp has already been translated and typed.
	      (and (caddr clause)
		   (if (or (zctype>array-p (cadr clause))
			   (zctype>struct-p (cadr clause)))
		       ;; STORE-VALUE assumes aggregates pre-exist; can't use here.
		       `((setq ,(car clause) ,(caddr clause)))
		     ;; Pointers have to be rplaca/d'ed, and chars masked.
		     (zcprim>standard-coercions
		       (zcprim>store-value
			 (zcprim>variable-reference (car clause) env context)
			 (cadr clause) (caddr clause) (cadr clause) env context)
		       (cadr clause) context nil))))
	  init-clauses))

(defun zcprim>auto-freelist-clauses (auto-inits)
  "Given a list of the form (<var> <type> <init> [<freelist-var>]), pulls out the
   ones with freelist variables (this list will be the return value), and arranges
   for the static-initials to get done at top level."
  (let ((freelist-clauses (subset #'(lambda (clause) (cdddr clause)) auto-inits)))
    (mapc #'(lambda (clause)
	       (let ((freelist-var (cadddr clause)))
		 (push `(,freelist-var nil) zcprim>*defun-static-inits*)))
	   freelist-clauses)
    freelist-clauses))

(defun zcprim>auto-freelist-cleanups (clauses env)
  "Given a list of clauses of the form (<var> <type> <init> <freelist-var>),
   returns a list of forms to push the aggregates back onto the freelists."
  (mapcan #'(lambda (clause)
	      (let ((local (car clause))
		    (type (cadr clause))
		    (freelist-var (cadddr clause)))
		(when (zerop (zctype>sizeof-in-scale type env))
		  (zcerror "Array or structure must have at least one element;~@
			    type ~A has none." type))
		`((setf (zcprim>array-freelist-link ,local) ,freelist-var)
		  (setq ,freelist-var ,local))))
	  clauses))

(defun zcprim>setup-locals (auto-inits env cleanups)
  "Given a list of auto-inits, returns four values: let-clauses to be bound around
   the PROG, variables to be bound in the PROG, cleanup forms to be executed in an
   unwind-protect after the PROG (whose tail is the supplied CLEANUPS), and
   additional initialization forms to be done at the beginning of the PROG."
  (zcprim>setup-locals-1 auto-inits env nil nil cleanups nil))
(defun zcprim>setup-locals-1 (auto-inits env let-clauses progvars cleanups
			      addl-inits)
  (if (null auto-inits) (values let-clauses progvars cleanups addl-inits)
    (let ((vars (if (zctype>canonicalized-pointer-p (cadar auto-inits))
		    (nlet ((array index
			    (zcprim>pointer-var-pair (caar auto-inits))))
		      `(,array ,index))
		  `(,(caar auto-inits)))))
      (if (null (zcenv>annotation (caar auto-inits) 'address-taken env))
	  (zcprim>setup-locals-1 (cdr auto-inits) env let-clauses
				 `(,@progvars ,@vars) cleanups addl-inits)
	(nlet ((address (zcprim>variable-address-var (caar auto-inits)))
	       ((aa-init aa-cleanups
		 (zcprim>bind-local-address-array address vars
						  (cadar auto-inits) env))))
	  (zcprim>setup-locals-1
	    (cdr auto-inits) env `(,@let-clauses ,@vars (,aa-init)) progvars
	    `(,@aa-cleanups . ,cleanups)
	    `(,@(and (zctype>shorter-than-int-p (cadar auto-inits))
		     (null (caddar auto-inits))
		     ;; Uninitialized char or short whose address is taken.  We
		     ;; must initialize it to avoid array-word-not-fixnum errors.
		     `((setq ,(caar auto-inits) 0)))
	      . ,addl-inits)))))))

(defun zcprim>bind-local-address-array (address-var vars type env)
  "Returns a let-clause that binds ADDRESS-VAR to a local address array for VARS,
   which is a list of either a single variable, or a pointer pair."
  ;; This conditionalization actually depends on the compiler, not the hardware.
  ;; However neither LMI nor TI seem to have modified the compiler in a way that
  ;; affects this (i.e., locals are still allocated as named).
  #+3600
  (if (= (length vars) 2)
      ;; This crazy hack apparently suffices to keep the variable pair in
      ;; successive locations on the stack.
      (zcprim>bind-temp-displaced-array
	address-var `(or (locf ,(car vars)) (locf ,(cadr vars))) (length vars)
	(zcprim>scale-art (zctype>type-scale type))
	`(,(car vars) ,(zctype>array-of type 1) ,env) 'value-cell)
    (zcprim>bind-temp-displaced-array
      address-var `(locf ,(car vars)) (length vars)
      (zcprim>scale-art (zctype>type-scale type))
      `(,(car vars) ,(zctype>array-of type 1) ,env) 'value-cell))
  #-3600
  (zcprim>bind-temp-displaced-array
    address-var `(locf ,(car vars)) (length vars)
    (zcprim>scale-art (zctype>type-scale type))
    `(,(car vars) ,(zctype>array-of type 1) ,env) 'value-cell))

(defun zcprim>setup-setjmps (body setjmps)
  "If necessary, wraps code around this body to simulate the (ugly!) behavior of
   SETJMP in C."
  (if (null setjmps) body
    (let ((setjmps (mapcar #'(lambda (tag) `(,tag (go ,tag)))
			   setjmps)))
      `((do ((.setjmp-tag. t) .setjmp-return-value.)
	    ((null .setjmp-tag.))
	  (setf (values .setjmp-return-value. .setjmp-tag.)
		,(do ((body `(prog () (selectq .setjmp-tag. . ,setjmps)
				. ,body)
			    `(#+Symbolics catch #-Symbolics *catch
				',(caar setjmps) ,body))
		      (setjmps setjmps (cdr setjmps)))
		     ((null setjmps) body))))))))

(defprim c:|goto|
  ((&quote label)
   `((go ,label))))

(defprim c:label+
  ((&quote label &eval (stmt stype :statement))
   (ignore stype)
   (cons label stmt)))

(defprim c:|while|
  (((pred ptype :boolean) &quote &body body)
   (ignore ptype)
   (nlet ((break-label (zcprim>gen-label))
	  (continue-label (zcprim>gen-label))
	  (loop-label (zcprim>gen-label))
	  ((body (zcprim>translate-block
		   body **env **context
		   ':break break-label ':continue continue-label))))
     `((go ,continue-label)
       ,loop-label
       ,@body
       ,continue-label
       (if ,pred (go ,loop-label))
       ,break-label))))

(defprim c:|do|
  ((&quote stmt &eval (pred ptype :boolean))
   (ignore ptype)
   (nlet ((loop-label (zcprim>gen-label))
	  (break-label (zcprim>gen-label))
	  (continue-label (zcprim>gen-label))
	  ((body (zcprim>translate-block
		   (ncons stmt) **env **context
		   ':break break-label ':continue continue-label))))
     `(,loop-label
       ,@body
       ,continue-label
       (if ,pred (go ,loop-label))
       ,break-label))))

(defprim c:|for|
  (((init inittype :statement) (pred ptype :boolean) (incr inctype :statement)
    &quote &body body)
   (ignore inittype ptype inctype)
   (nlet ((break-label (zcprim>gen-label))
	  (continue-label (zcprim>gen-label))
	  (loop-label (zcprim>gen-label))
	  (test-label (zcprim>gen-label))
	  ((body (zcprim>translate-block
		   body **env **context
		   ':break break-label ':continue continue-label))))
     `(,@init
       (go ,test-label)
       ,loop-label
       ,@body
       ,continue-label
       ,@incr
       ,test-label
       (if ,pred (go ,loop-label))
       ,break-label))))

(defprim c:|break|
  (()
   (let ((break-label (cadr (gmap (:or) #'(lambda (frame) (memq ':break frame))
				  (:list **context)))))
     (if break-label
	 `((go ,break-label))
       (zcerror "BREAK not inside WHILE, FOR, DO, or SWITCH")))))

(defprim c:|continue|
  (()
   (let ((continue-label (cadr (gmap (:or)
				     #'(lambda (frame) (memq ':continue frame))
				     (:list **context)))))
     (if continue-label
	 `((go ,continue-label))
       (zcerror "CONTINUE not inside WHILE, FOR, or DO")))))

(defprim c:|return|
  (()
   `((return-from ,(zcprim>function-name **context)
       '(:|No value returned from| ,(zcprim>function-name **context)))))
  (((val valtype))
   (nlet ((funcname (zcprim>function-name **context))
	  (functype (zcprim>function-return-type **context))
	  (ret-temp (zcprim>function-ret-temp **context)))
     (cond ((not (zctype>match valtype functype **env))
	    (zcerror "Type mismatch between function declaration and RETURN value:
   ~A declared, ~A returned" functype valtype))
	   ((and (zctype>array-p valtype) (zcprim>local-p val **context))
	    (zcerror "Attempt to return (a pointer to) auto array ~A" val))
	   ((zctype>arith-pointer-p functype)
	    (nlet ((let-clauses array index
				(zcprim>analyze-pointer-exp val valtype)))
	      `(,(zcprim>let-form let-clauses
		   `(return-from ,funcname (values ,array ,index))))))
	   ((null ret-temp) `((return-from ,funcname ,val)))
	   ((symbolp val)
	    ;; Structures in variables don't need to be copied.
	    `((return-from ,funcname (values ,val 0))))
	   (t
	    (nlet ((store-exp (zcprim>store-value ret-temp functype val valtype
						  **env **context))
		   ((let-clauses array index
				 (zcprim>analyze-pointer-exp store-exp valtype))))
	      `(,(zcprim>let-form let-clauses
		   `(return-from ,funcname (values ,array ,index))))))))))

(defprim c:|switch|
  (((arg type) &quote &body body)
   (if (not (zctype>integer-p type))
       (zcerror "Wrong argument type to SWITCH: ~A (an integer type is required)"
		type))
   (if (not (and (listp (car body)) (eq (caar body) 'c:block+) (null (cadar body))
		 (null (cdr body))))
       (zcerror "The body of a switch statement must be a block, with no
  declarations (the latter is a Zeta-C restriction)")
     (nlet ((body (cddar body))
	    (break-label (zcprim>gen-label))
	    ((labels explicit-default (zcprim>switch-labels body))
	     ((default-label (or explicit-default break-label)))))
       `(,(zcprim>switch-selectq arg body labels default-label)
	 (go ,default-label)
	 ,@(zcprim>switch-prog body labels **env
			       (cons (list ':statement ':break break-label)
				     **context))
	 ,break-label)))))

(defun zcprim>switch-labels (body)
  ; I'd like to use LetS for this (and others below) but I don't know it yet.
  (do ((body body (cdr body))
       (labels nil)
       (explicit-default nil))
      ((null body) (values (nreverse labels) explicit-default))
    (when (or (and (listp (car body)) (eq (caar body) 'c:|case|))
	      (eq (car body) 'c:|default|))
      (push (zcprim>gen-label) labels)
      (when (eq (car body) 'c:|default|)
	(setq explicit-default (car labels))))))

(defun zcprim>switch-selectq (arg body symlist default-label)
  (do ((body body (cdr body))
       (clauses nil))
      ((null body)
       (let ((clauses (sortcar clauses #'<)))
	 (if (zcprim>switch-binary-p clauses)
	     (zcprim>switch-binary-search arg clauses default-label)
	   (and clauses `(selectq ,arg . ,clauses)))))
    (cond ((and (listp (car body)) (eq (caar body) 'c:|case|))
	   (push `(,(zcprim>eval-constant-int (cadar body)) (go ,(pop symlist)))
		 clauses))
	  ((eq (car body) 'c:|default|)
	   (pop symlist)))))	  ; "otherwise" handled elsewhere.

(defun zcprim>switch-binary-p (clauses)
  "Applies a heuristic to a list of switch clauses to determine whether binary
   search is likely to be a good idea.  The current heuristic is: there must be at
   least 8 clauses, and there have to be gaps between not more than 40% of the
   consecutive pairs of clauses."
  (do ((clauses clauses (cdr clauses))
       (length 1 (1+ length))
       (skips 0 (if (> (caadr clauses) (1+ (caar clauses)))
		    (1+ skips) skips)))
      ((null (cdr clauses))
       (and ( length 8)
	    ( (// (float skips) (1- length)) .4s0)))))

(defun zcprim>switch-binary-search (arg clauses default-label)
  "Generates an open-coded binary search to find the value of ARG in a fixed list
   of integers, which are the cars of CLAUSES.  The resulting code will be a tree
   of IFs that will execute the cadr of the clause whose car is = to ARG."
  (nlet ((var let-clauses (zcprim>make-temp-if-needed arg))
	 (min (caar clauses))
	 (max (caar (last clauses)))
	 ((tree (zcprim>switch-binary-search-1 var clauses (length clauses) min max
					       `(go ,default-label)))))
    (zcprim>let-form let-clauses
      `(if (and ( ,var ,min) ( ,var ,max)) ,tree (go ,default-label)))))
(defun zcprim>switch-binary-search-1 (var clauses length min max default)
  "Generates an open-coded binary search to find the value of VAR in a fixed list
   of integers, which are the cars of CLAUSES (which is assumed to have length
   LENGTH).  MIN and MAX are (inclusive) lower and upper bounds; this function
   assumes that the value of VAR is between them.  The resulting code will be a
   tree of IFs that will execute the cadr of the clause whose car is = to VAR."
  (if (= length 1)
      (if (and (= min max) (= min (caar clauses)))
	  (cadar clauses)
	`(if (= ,var ,(caar clauses)) ,(cadar clauses) ,default))
    (nlet ((left-length (// length 2))
	   ((right-clauses (nthcdr left-length clauses))
	    ((test-val (caar right-clauses)))))
      `(if (< ,var ,test-val)
	   ,(zcprim>switch-binary-search-1 var clauses left-length
					   min (1- test-val) default)
	 ,(zcprim>switch-binary-search-1 var right-clauses (- length left-length)
					 test-val max default)))))

(defun zcprim>switch-prog (body labels env context)
  (let ((forms nil))
    (do ((body body (cdr body))
	 (next-form (locf forms)))
	((null body) forms)
      (cond ((or (and (listp (car body)) (eq (caar body) 'c:|case|))
		 (eq (car body) 'c:|default|))
	     (rplacd next-form (ncons (pop labels)))
	     (setq next-form (locf (cddr next-form))))
	    (t
	     (rplacd next-form (zcmac>translate-exp (car body) env context))
	     (if (cdr next-form)
		 (setq next-form (locf (cdr (last (cdr next-form)))))))))))

(defprim c:|case|
  ((&quote ignore)
   (zcerror "CASE not inside SWITCH")))

(defprim c:|setjmp|
  (((jb-exp jb-type))
   (when (not (zctype>lispval-p jb-type))
     (zcerror "Wrong type argument, /"~A/", to setjmp"
	      (zclstn>type-string jb-type)))
   (when (second (assq ':statement **context))
     (zcerror "ZETA-C restriction: only one SETJMP call allowed in a statement"))
   (let ((label (intern (zcprim>gen-label))))
     (push label (cdr (assq ':setjmps (cdr (assq ':d-block **context)))))
     (setf (second (assq ':statement **context))
	   `((setq .setjmp-return-value. 0)
	     ,label))
     (values `(progn ,(zcprim>store-value jb-exp jb-type `',label (zctype>lispval)
					  **env **context)
		     .setjmp-return-value.)
	     (zctype>int)))))


; ================================================================
; Function definition.

(defmacro zcprim>with-option-bindings (&body body)
  "Binds those ZETA-C option variables which are specified by a #define in the
   source."
  `(let ((*suppress-auto-float-to-double*
	   (zcenv>#definition (intern "ZETA_C_SUPPRESS_AUTO_FLOAT_TO_DOUBLE")
			      *source-location*))
	 (*compare-incomparable-pointers*
	   (zcenv>#definition (intern "ZETA_C_COMPARE_INCOMPARABLE_POINTERS")
			      *source-location*)))
     . ,body))

; This is a macro, not a prim, because it can only be used at top level.
(defmacro c:defunc+ (*source-location* func-decl param-decls body)
  "Defines a user function in a Zeta-C program."
  (nlet ((package (if (zcenv>source-loc-file) (symbol-package (zcenv>source-loc-file))
		    package))
	 (env (zcenv>create-env (zcenv>global-env)))
	 ((func type params func-type-declarers
		(zcdecl>function-declaration func-decl param-decls env)))
	 (((zcprim>*expanding-defunc+* func)))
	 (zcprim>*defun-specials* nil)
	 (zcprim>*defun-static-inits* nil)
	 (zcprim>*defun-cleanup-forms* nil)
	 (zcprim>*defun-toplevel-forms* nil)
	 (zcprim>*defun-function-call-info* nil)
	 (((hook (zcprim>defunc+-hook func type env))))
	 #+Symbolics
	 (save-rfdw compiler:*reset-function-definition-warnings*))
    ;; Arrange to keep our compiler warnings from getting discarded.
    ;; (Symbolics version -- test in LMIT & TI systems)
    #+Symbolics
    (unless compiler:compiling-whole-file-p
      (and compiler:*reset-function-definition-warnings*
	   (compiler:reset-function-warnings func 'defun)
	   (setq compiler:*reset-function-definition-warnings* nil)))
    (zcprim>with-option-bindings
      (nlet ((ret-temp (zcprim>defun-return-hack (zctype>function-return-type type)
						 env))
	     ((context `((:defunc ,func ,type ,params ,ret-temp)))
	      ((lambda-list body
		(zcprim>process-defun-body func params body env context)))))
	`(progn
	   'compile
	   (zcprim>initialize ',zcprim>*defun-static-inits*)
	   ,@zcprim>*defun-toplevel-forms*
	   ,(if hook
		(funcall hook :defun-form nil func lambda-list body)
	      `(defun ,func ,lambda-list . ,body))
	   ,@func-type-declarers
	   (zcprim>record-call-info ',zcprim>*defun-function-call-info*)
	   (zcprim>load-check-arg-types ',func ',*source-location*)
	   #+Symbolics
	   ,@(and save-rfdw
		  `((setq compiler:*reset-function-definition-warnings* t)))
	   ',func)))))

(defun zcprim>process-param-list (params env)
  "Inserts &optional before the first param declared as an optarg; if the last
   of the params is declared as a restarg, inserts &rest before it.  Also,
   substitutes an array-index pair for pointer parameters."
  (nlet ((last-param (car (last params)))
	 ((params (if (eq (zcenv>get last-param 'arghack env) ':restarg)
		      (nconc (butlast params) (list '&rest last-param))
		    params))))
    (zcprim>process-param-list-1 params env nil)))
(defun zcprim>process-param-list-1 (params env optarg-found-p)
  (cond ;; Don't pointer-pair-ize a &rest arg.
	((or (null params) (eq (car params) '&rest)) params)
	((and (not optarg-found-p)
	      (eq (zcenv>get (car params) 'arghack env) ':optarg))
	 (cons '&optional (zcprim>process-param-list-2 params env t)))
	(t (zcprim>process-param-list-2 params env optarg-found-p))))
(defun zcprim>process-param-list-2 (params env optarg-found-p)
  (let ((type (zcenv>type (car params) env)))
    (if (zctype>arith-pointer-p type)
	(nlet ((array-sym index-sym (zcprim>pointer-var-pair (car params))))
	  (list* (if (not optarg-found-p) array-sym (list array-sym nil))
		 (if (not optarg-found-p) index-sym (list index-sym 0))
		 (zcprim>process-param-list-1 (cdr params) env optarg-found-p)))
      (cons (if (not optarg-found-p) (car params)
	      (list (car params)
		    (cond ((zctype>double-p type) #+3600 0.0d0 #-3600 0.0)
			  ((zctype>float-p type) #+3600 0.0 #-3600 0.0s0)
			  ((zctype>integer-p type) 0)
			  (t nil))))
	    (zcprim>process-param-list-1 (cdr params) env optarg-found-p)))))

(defun zcprim>defun-return-hack (ret-type env)
  "Functions that return a structure keep an instance around to copy into and
   return.  This function sets such things up.  Returns the special variable bound
   to the instance, or else NIL."
  (and (zctype>struct-p ret-type)
       (or *firstclass-structures*
	   (zcerror "Returning structures from functions is not allowed.  If you would like to
  permit it (for Unix compatibility), setq zeta-c:*firstclass-structures* to T."))
       (let ((ret-temp (zcprim>gen-var)))
	 (push `(,ret-temp (zcdecl>create-or-reuse-structure nil ',ret-type ',env))
	       zcprim>*defun-static-inits*)
	 ret-temp)))

(defun zcprim>process-defun-body (func params body env context)
  "Internal to C:DEFUNC+."
  (nlet ((lambda-list (zcprim>process-param-list params env))
	 ;; The order of doing things here is important.  Innermost is the PROG
	 ;; containing the translated C statements...
	 (body `(prog ,func ()
		      ,@(zcmac>translate-exp body env context)
		      (return-from ,func '(:|No value returned from| ,func))))
	 ;; Parameter massaging is next (only because it adds cleanup-forms).
	 ((argument-lets (zcprim>massage-params params env))
	  ;; Around that goes the UNWIND-PROTECT, if any.
	  ((body (if (null zcprim>*defun-cleanup-forms*) body
		   `(unwind-protect ,body . ,zcprim>*defun-cleanup-forms*)))
	   ;; And again: the argument twiddling.
	   ((body (zcprim>let-form argument-lets body))
	    ;; And finally, the declarations.
	    ((body `((declare
		       (unspecial
			 . ,(rem-if #'(lambda (x) (memq x '(&optional &rest)))
				    lambda-list))
		       (special . ,zcprim>*defun-specials*)
		       (special . ,(mapcar #'car zcprim>*defun-static-inits*)))
		     ,body)))))))
    (values lambda-list body)))

(defun zcprim>massage-params (params env)
  "Returns a list of let-clauses that do any necessary coercions on parameters."
  (zcprim>massage-params-1 params env nil))
(defun zcprim>massage-params-1 (params env clauses)
  (if (null params) (nreverse clauses)
    (nlet ((param (car params))
	   ((type (zcenv>type param env))))
      (cond ((and (null (cdr params))
		  (eq (zcenv>get param 'arghack env) ':restarg))
	     (cons (zcprim>restarg-init param) (nreverse clauses)))
	    ((zctype>struct-p type)
	     (zcprim>massage-params-1
	       (cdr params) env
	       `(,(zcprim>struct-arg-init param type env) . ,clauses)))
	    ((zctype>shorter-than-int-p type)
	     (zcprim>massage-params-1
	       (cdr params) env
	       `((,param ,(zcprim>shorten-if-necessary param type nil))
		 . ,clauses)))
	    ((zcenv>annotation param 'address-taken env)
	     (nlet ((var length
			 (if (zctype>canonicalized-pointer-p type)
			     (values (zcprim>pointer-var-pair param) 2)
			   (values param 1)))
		    (address (zcprim>variable-address-var param))
		    ((aa-init aa-cleanups
		      (zcprim>bind-temp-displaced-array
			address `(locf ,var) length
			(zcprim>scale-art (zctype>type-scale type))
			`(,param ,(zctype>array-of type 1) ,env) 'value-cell))))
	       (setq zcprim>*defun-cleanup-forms*
		     (nconc aa-cleanups zcprim>*defun-cleanup-forms*))
	       (zcprim>massage-params-1 (cdr params) env `(,aa-init . ,clauses))))
	    (t (zcprim>massage-params-1 (cdr params) env clauses))))))

(defun zcprim>restarg-init (var)
  "Creates a let-clause for the restarg VAR that makes an indirect array that
   points to the &rest list on the stack (reusing an array header from the
   freelist, if any)."
  (nlet ((let-clause cleanups
	  (zcprim>bind-temp-displaced-array
	    var `(%make-pointer dtp-locative ,var)
	    `(length ,var) 'art-q `(,var nil nil) 'restarg)))
    (setq zcprim>*defun-cleanup-forms*
	  (nconc cleanups zcprim>*defun-cleanup-forms*))
    let-clause))

(defun zcprim>defunc+-hook (func type env)
  (let ((hook (and (symbolp func) (get func 'defunc+-hook))))
    (and hook (funcall hook :begin-definition nil func type env))))

(defun zcprim>bind-temp-displaced-array (var displaced-to length art id nss)
  "Returns an expression that binds VAR to a temporary array which is displaced to
   DISPLACED-TO, of length LENGTH (the latter two are expressions).  ART is the
   array-type symbol; ID is the id-list of the array (leader 0); NSS is the
   named-structure-symbol.  Second value is a list of cleanup-forms."
  (let ((freelist-var (zcprim>gen-var)))
    (push `(,freelist-var nil) zcprim>*defun-static-inits*)
    (values
      `(,var
	(let ((%%temp (or ,freelist-var (zcprim>make-temp-displaced-array))))
	  ;; We link the freelist together through an array-leader elt.
	  (setq ,freelist-var (zcprim>array-freelist-link %%temp))
	  (zcprim>init-temp-displaced-array %%temp ,displaced-to ,length ',art
					    ',id ',nss)))
      `((setf (zcprim>array-freelist-link ,var) ,freelist-var)
	(setq ,freelist-var ,var)))))

#+3600
(defun zcprim>make-temp-displaced-array ()
  (si:inhibit-gc-flips
    ;; MAKE-ARRAY is very slow.
    ;; This changed slightly for Rel7 (the index-offset field is now mandatory)
    ;; but the change might be back-compatible (assuming not though).
    (let ((array (si:%allocate-structure-block #+Genera 11 #-Genera 9
					       default-cons-area)))
      (si:%p-store-cdr-type-and-pointer array si:%header-type-array
					si:dtp-header-i 0)
      (setf (si:array-dispatch-field array) si:%array-dispatch-long)
      (setf (si:array-type-field array) art-q)
      (setf (si:array-indirect-pointer array) nil)
      #+Genera (setf (si:array-index-offset-field array) 0)
      (setf (si:array-long-prefix-length-field array) #+Genera 4 #-Genera 3)
      (setf (si:array-long-leader-length-field array) (zcprim>array-leader-length))
      (setf (si:array-dimensions-field array) 1)
      (setf (si:array-long-length-field array) 1)
      (setf (si:array-named-structure-bit array) 1)
      (%p-store-data-type (locf array) si:dtp-array)
      (setf (zcprim>array-freelist-link array) nil)
      array)))

#-3600
(defun zcprim>make-temp-displaced-array ()
  ;; MAKE-ARRAY is very very slow.
  (let ((array (%allocate-and-initialize-array
		 '#,(+ 2 (%logdpb 1 si:%%array-displaced-bit
				  (%logdpb 1 si:%%array-number-dimensions
					   (%logdpb 1 si:%%array-leader-bit
						    art-q))))
		 0 (zcprim>array-leader-length) default-cons-area
		 (+ 5 (zcprim>array-leader-length)))))
    (%p-store-contents-offset nil array 1)
    (%p-store-contents-offset 0 array 2)
    (setf (zcprim>array-freelist-link array) nil)
    array))

(defun zcprim>init-temp-displaced-array (array displaced-to length type id nss)
  ;; SI:CHANGE-INDIRECT-ARRAY is very slow -- calls EVAL??!
  (dotimes (i (array-leader-length array)) (store-array-leader nil array i))
  (store-array-leader id array 0)
  (store-array-leader nss array 1)
  #+Symbolics
  (progn (setf (si:array-indirect-pointer array) displaced-to)
	 (setf (si:array-long-length-field array) length)
	 (setf (si:array-type-field array) (symeval type)))
  #-Symbolics
  (progn (%p-store-contents-offset displaced-to array 1)
	 (%p-store-contents-offset length array 2)
	 (%p-deposit-field (symeval type) si:%%array-type-field array))
  (zcprim>store-scale-slot (zcprim>art-scale type) array array)
  array)

; Should this do stack arrays on the 3600?  This is probably faster...
(defun zcprim>struct-arg-init (param type env)
  (let ((freelist-var (zcprim>gen-var)))
    (push `(,freelist-var nil) zcprim>*defun-static-inits*)
    ;; We chain the freelist through an element of the array-leader.
    (push `(progn (setf (zcprim>array-freelist-link ,param) ,freelist-var)
		  (setq ,freelist-var ,param))
	  zcprim>*defun-cleanup-forms*)
    `(,param
      (let ((%%temp (or ,freelist-var
			(zcdecl>create-or-reuse-structure nil ',type ',env))))
	(setq ,freelist-var (zcprim>array-freelist-link %%temp))
	(setf (car (zcprim>array-desc %%temp)) ',param)
	,@(zcprim>exp-to-statement (zcprim>structure-assign param '%%temp type env)
				   type)
	%%temp))))

(defun zcprim>function-name (context)
  "What is the name of the function being compiled?"
  (second (assq ':defunc context)))

(defun zcprim>function-return-type (context)
  "The type that the function begin compiled is declared to return."
  (zctype>function-return-type (third (assq ':defunc context))))

(defun zcprim>param-p (sym context)
  "Is SYM a parameter to the function being compiled?"
  (memq sym (fourth (assq ':defunc context))))

(defun zcprim>function-ret-temp (context)
  "For functions returning pointers or structures, the variable bound to the
   temporary instance; else NIL."
  (fifth (assq ':defunc context)))

(defun zcprim>local-p (sym context)
  "Is SYM a bound local in this context?"
  (gmap (:or) #'(lambda (frame)
		  (and (eq (car frame) ':locals)
		       (assq sym (cdr frame))))
	(:list context)))


; ================================================================
; External declarations, system type definition, and misc.

(defmacro c:decl+ (*source-location* &rest decl)
  (zcprim>with-option-bindings
    (nlet ((package (if (zcenv>source-loc-file)
			(symbol-package (zcenv>source-loc-file))
		      package))
	   ((result-list top-level-forms storage-class
			 (zcdecl>external-declaration decl))))
      `(progn
	 'compile
	 (eval-when (eval compile load) . ,top-level-forms)
	 . ,(and (neq storage-class ':extern)
		 `((eval-when (compile)
		     (special . ,(mapcar #'car result-list)))
		   . ,(and result-list
			   `((eval-when (eval load)
			       (zcprim>initialize ',result-list))))))))))

(defun zcprim>extern (vars)
  (mapc #'(lambda (var)
	    (unless (get var 'special) (putprop var t 'special))
	    (let ((type (zcenv>type var (zcenv>global-env))))
	      (when (and (not (boundp var)) (zctype>array-p type))
		(set var
		     (make-array 0 :leader-list (zcprim>array-leader-init
						  var type (zcenv>global-env))
				 :named-structure-symbol 'extern-dummy)))))
	vars))

(defun zcprim>initialize (inits)
  (mapc #'zcprim>initialize-one inits))

(defun zcprim>initialize-one (init)
  (let ((var (car init))
	(init-exp (cadr init)))
    (when (si:record-source-file-name var 'defvar)
      (putprop var (or si:fdefine-file-pathname t) 'special)
      (when (cdr init)
	(set var (eval init-exp))
	(zcprim>add-initialization var init-exp 'value-initializer)))))

(defun zcprim>add-initialization (name init-form type)
  "Adds an initialization for a variable or function NAME to the list for this
   package.  TYPE should be either 'VALUE-INITIALIZER or 'FUNCTION-INITIALIZER."
  (putprop name init-form type)
  (let ((pkg-pair (or (assq package *package-variable-initialization-alist*)
		      (let ((pr (ncons package)))
			(push pr *package-variable-initialization-alist*)
			pr))))
    (or (memq name (cdr pkg-pair))
	(tail-push name (cdr pkg-pair)))))

(defprim c:cast+
  ((&quote type orig-exp)
   (nlet ((type (zcdecl>invert-abstract-decl type **env))
	  (**context (cons nil **context))
	  ((mexp mtype
	     (zcprim>cast-allocation-magic type orig-exp **env **context))))
     (if mexp (values mexp mtype)
       (nlet ((exp exp-type (zcmac>translate-exp orig-exp **env **context)))
	 (zcprim>cast exp exp-type type **env))))))

; Yeesh, this is huge.  But there really are all these cases.
(defun zcprim>cast (exp exp-type type **env)
  (cond ;; Special case for null pointers.
    ((and (zctype>arith-pointer-p type) (zctype>zero-p exp-type))
     (values (zcprim>null-pointer)
	     (zctype>null-pointer (zctype>pointer-deref-type type))))
    ((and (zctype>word-p exp-type) (zctype>arith-pointer-p type))
     ;; An int (or lispval) to a pointer -- see if the int already holds a cons.
     (nlet ((exp let-clauses (zcprim>make-temp-if-needed exp))
	    (array (zcprim>gen-var))
	    (index (zcprim>gen-var))
	    ((a&i-lets
	       `((,array ,index
		  ,(if (numberp exp) `(values nil ,exp)
		     `(if (zcptr>ptr-p ,exp)
			  (values (zcptr>array ,exp) (zcptr>index ,exp))
			(values nil ,exp))))))))
       (values (zcprim>let-form
		 (zcprim>subordinate-nlet-clauses let-clauses a&i-lets)
		 `(zcptr>cons ,array ,index))
	       (zctype>canonicalize-pointer type))))
    ((and (zctype>arith-pointer-p exp-type) (zctype>word-p type))
     ;; A pointer to an int (or lispval) -- see if the array is NIL.
     (nlet ((let-clauses array index
			 (zcprim>analyze-pointer-exp exp exp-type)))
       (values (zcprim>let-form let-clauses
		 (if (stringp array)
		     `(zcptr>cons ,array ,index)
		   (if array `(if ,array (zcptr>cons ,array ,index) ,index)
		     index)))
	       type)))
    ((and (zctype>arith-pointer-p exp-type) (zctype>arith-pointer-p type))
     (nlet ((edtype (zctype>pointer-deref-type exp-type))
	    ((escale (zctype>type-scale edtype))
	     (evoidp (zctype>void-p edtype)))
	    (ddtype (zctype>pointer-deref-type type))
	    ((dscale (zctype>type-scale ddtype))
	     (dvoidp (zctype>void-p ddtype))))
       (values
	 (nlet ((let-clauses array index (zcprim>analyze-pointer-exp exp exp-type))
		((array (cond (dvoidp array)
			      (evoidp `(and ,array
					    (zcprim>array-as-scale ',dscale ,array)))
			      (t (zcprim>rescale-array escale dscale array))))
		 (index (cond (dvoidp index)
			      (evoidp `(if (null ,array) ,index
					 (// (* ,index
						(zctype>scale-size
						  (zcprim>array-scale ,array)))
					     (zctype>scale-size ',dscale))))
			      (t (zcprim>rescale-index escale dscale index))))))
	   (zcprim>let-form let-clauses
	     `(zcptr>cons
		,(if (and (zctype>struct-p ddtype)
			  (not (zctype>struct-p edtype)))
		     `(zcprim>fill-scale-slots ,array) array)
		,index)))
	 type)))
    ((and (zctype>function-pointer-p exp-type) (zctype>function-pointer-p type))
     (values exp type))
    ;; When casting a normal pointer to a pointer-to-function, we look at the
    ;; array: if NIL, we generate a null function pointer.
    ((and (zctype>pointer-p exp-type) (zctype>function-pointer-p type))
     (nlet ((let-clauses array
	     (zcprim>analyze-pointer-exp exp exp-type :must-bind nil)))
       (values (zcprim>let-form let-clauses `(or ,array 'null-function-pointer))
	       type)))
    ;; ... and when casting back to a normal pointer, we check for the null pf.
    ((and (zctype>function-pointer-p exp-type) (zctype>pointer-p type))
     (nlet ((exp let-clauses (zcprim>make-temp-if-needed exp)))
       (values (zcprim>let-form let-clauses
		 `(zcptr>cons (and (neq ,exp 'null-function-pointer) ,exp) 0))
	       type)))
    ;; Similarly for casting numbers and lispvals to and from function pointers.
    ((and (zctype>word-p exp-type) (zctype>function-pointer-p type))
     (nlet ((exp let-clauses (zcprim>make-temp-if-needed exp)))
       (values (zcprim>let-form let-clauses
		 `(if ,(if (zctype>number-p exp-type) `(eql ,exp 0) `(null ,exp))
		      'null-function-pointer
		    ,exp))
	       type)))
    ((and (zctype>function-pointer-p exp-type) (zctype>word-p type))
     (nlet ((exp let-clauses (zcprim>make-temp-if-needed exp)))
       (values (zcprim>let-form let-clauses
		 `(if (neq ,exp 'null-function-pointer) ,exp
		    ,(if (zctype>number-p type) 0 nil)))
	       type)))
    ((and (zctype>number-p exp-type) (zctype>number-p type))
     (values (zcprim>shorten-if-necessary
	       (zcprim>coerce-numbers exp exp-type type)
	       type nil)
	     type))
    ((or (zctype>match exp-type type **env)
	 (zctype>lispval-p type)
	 (zctype>lispval-p exp-type))
     (values exp type))
    ((zctype>void-p type)
     (values `(prog1 nil . ,(zcprim>exp-to-statement exp exp-type)) type))
    (t (zcerror "Can't cast type ~A to type ~A" exp-type type))))

(defun zcprim>cast-allocation-magic (type orig-exp env context)
  (let ((dtype (and (zctype>arith-pointer-p type)
		    (zctype>pointer-deref-type type))))
    (and dtype
	 (listp orig-exp)
	 (cond ((and (= (length orig-exp) 3)	     ; Error checking.
		     (eq (car orig-exp) 'c:|calloc|)
		     (eql (zctype>sizeof dtype env)
			  (zcmac>translate-exp (caddr orig-exp) env context)))
		;; It's a CALLOC call whose elt-size is the size of the deref of
		;; the type being cast to.
		(values `(zcptr>cons (zcprim>allocate-array
				       ,(zcmac>translate-exp (cadr orig-exp) env context)
				       ',dtype ',env)
				     0)
			type))
	       ((and (= (length orig-exp) 2)
		     (eq (car orig-exp) 'c:|malloc|))
		;; It's a MALLOC call.
		(values `(zcptr>cons (zcprim>allocate-object
				       ,(zcmac>translate-exp (cadr orig-exp) env context)
				       ',dtype ',env)
				     0)
			type))))))

(defun zcprim>allocate-array (nelts type env)
  (zcdecl>create-or-reuse-structure nil (zctype>array-of type nelts) env))

(defun zcprim>allocate-object (nbytes type env)
  "Allocate an object of size NBYTES, making it appear to be of type TYPE.  This
   is to be substituted for MALLOC calls whose results are cast to a known type."
  (nlet ((scale  (zctype>type-scale type))
	 (nss (if (not (zctype>struct-p type)) 'array
		(or (zctype>struct-tag type)
		    (cdr (assq (zctype>struct-class type)
			       '((:struct . struct) (:packed-struct . struct)
				 (:union . union)))))))
	 ((size (zctype>rescale-index nbytes :8B scale))
	  (art (zcprim>scale-art scale))
	  ((objtype (zctype>array-of type
				     (// size (zctype>sizeof-in-scale type env))))
	   ((obj (make-array size :type art :initial-value 0
			     :named-structure-symbol nss
			     :leader-list
			     (zcprim>array-leader-init nil objtype env)))))))
    (zcprim>store-scale-slot scale obj obj)))

(defun zcprim>null-pointer ()
  "Returns the canonical null pointer."
  ''(nil . 0))

(defun zcprim>null-function-pointer ()
  "Returns an expression for the canonical null function pointer."
  ''null-function-pointer)

(defprim c:|sizeof|
  (((exp exp-type))
   (ignore exp)
   (zcprim>sizeof exp-type **env))
  ((&quote type type-p)
   (ignore type-p)			; Second arg is only to distinguish cases.
   (let ((type (zcdecl>invert-abstract-decl type **env)))
     (zcprim>sizeof type **env))))

(defun zcprim>sizeof (type env)
  (when (or (and (zctype>array-p type) (null (zctype>array-length type)))
	    (zctype>function-p type)
	    (zctype>void-p type))
    (zcerror "Can't take SIZEOF type ~A" type))
  (values (zctype>sizeof type env) (zctype>int)))

;;; Necessitated by Common Lisp strings.
(defprim c:string+
  "Converts a (Common) Lisp string into a C string."
  ((&quote str)
   (let ((cstr (string-to-C str)))
     (values `',cstr (zctype>constant-type cstr)))))

(defun zcprim>stringp (frob)
  "Is FROB the internal representation of a string constant?"
  (and (listp frob) (eq (car frob) 'c:string+)))

(defprim c:/#lisp
  "Inclusion of Lisp code.  The first argument is the type, the second is any Lisp
   form; unless the type is NIL, in which case there may be an arbitrary number of
   forms, which will be treated as statements."
  ((&quote type &body forms)
   (if (null type)
       (values forms nil)
     (values (car forms) (zcdecl>invert-abstract-decl type **env)))))

(defmacro c:/#lisp (type &body forms)
  "Top-level inclusion of Lisp code.  This returns a Lisp form rather than a list
   of translated expressions."
  (ignore type)
  `(progn . ,forms))

(defun zcprim>translate-for-top-level (c-form env)
  "Translates a C form for the C listener.  Returns two values: the translated form
   and its type."
  (declare (values trans-form type))
  (nlet ((trans-form type (zcprim>with-option-bindings
			    (zcmac>translate-exp c-form env '((:top-level))))))
    (if (zctype>canonicalized-pointer-p type)
	(nlet ((let-clauses array index
		(zcprim>analyze-pointer-exp trans-form type)))
	  ;; Won't hurt to cons at this level...
	  (values (zcprim>let-form let-clauses `(zcptr>cons ,array ,index)) type))
      (values trans-form type))))

(defprim c:quote+
  "A quoted constant, with its (translated) type."
  ((&quote const type)
   (cond ((zctype>arith-pointer-p type)
	  (if (and (zctype>array-p type) (arrayp const))
	      (values const type)
	    (values `(zcptr>cons ',(car const) ',(cdr const)) type)))
	 ((and (listp const) (fixp (cdr const)))
	  ;; In flat mode, aggregate-valued expressions can look like pointers.
	  (values `(zcptr>flat-deref (zcptr>cons ',(car const) ',(cdr const)))
		  type))
	 ((and (zctype>number-p type) (numberp const))
	  (values const type))
	 (t (values `',const type)))))

; Used by the C listener for the _variables.
(defun zcprim>declare-external (name type)
  "Declares external variable NAME, of type TYPE.  Performs all necessary
   initialization."
  (nlet ((inits eval-form
	  (zcdecl>declare-one-name name type nil ':external nil
				   (zcenv>global-env))))
    (eval eval-form)
    (mapc #'(lambda (init) (set (car init) (eval (cadr init)))) inits)))


; ================================================================
; Pointer manipulation assistance.

(defun zcprim>analyze-pointer-exp (exp type &optional &key (must-bind t))
  "Analyzes a pointer-valued expression to see if it is doing consing.  In any
   case, it returns four values: a list of let-clauses; an expression for the
   array of the pointer; an expression for the index of the pointer; and T if the
   expression is, in fact, consing.  The expressions are valid only in the context 
   of the bindings set up by the let-clauses.  MUST-BIND should be NIL only in
   those cases where only one of the array and index expressions will be used2,
   and even that only once.*
   RESTRICTION: if EXP is an NLET-expression, it must have only one form in
   its body!"
  (declare (values let-clauses array index conses-p))
  (zcprim>analyze-pointer-exp-f exp type must-bind))
(defun zcprim>analyze-pointer-exp-f (exp type must-bind)
  (bcond ((or (zctype>zero-p type)
	      (and (zctype>null-pointer-p type) (fixp exp)))
	  (values nil nil exp nil))
	 ((and (zctype>array-p type)
	       (or (nlistp exp) (eq (car exp) 'quote)))
	  (values nil exp 0 nil))
	 ((zctype>function-pointer-p type)
	  (zcerror "Internal error: ZCPRIM>ANALYZE-POINTER-EXP called on a function pointer"))
	 ((and (symbolp exp) (zctype>canonicalized-pointer-p type))
	  (nlet ((array index (zcprim>pointer-var-pair exp)))
	    (values nil array index t)))
	 ;; ZCPTR>FLAT-DEREF is an identity function, used only to distinguish
	 ;; this case from the previous one.
	 ((and (listp exp) (eq (car exp) 'zcptr>flat-deref))
	  (zcprim>analyze-pointer-exp-f (cadr exp)
					(zctype>canonicalize-pointer type)
					must-bind))
	 ((and (zctype>canonicalized-pointer-p type)
	       (listp exp) (eq (car exp) 'quote))
	  (values nil (caadr exp) (cdadr exp) nil))
	 ((zcprim>trivial-p exp)
	  (values nil `(zcptr>array ,exp) `(zcptr>index ,exp) nil))
	 ((eq (car exp) 'zcptr>cons)
	  (if (null (cdddr exp))
	      (values nil (second exp) (third exp) (car exp))
	    ;; Special case for bashing existing conses (REUSE arg to ZCPTR>CONS).
	    (nlet ((cons let-clauses
			 (if (zcprim>trivial-p (fourth exp))
			     (values (fourth exp) `((ignore ,exp)))
			   (zcprim>make-temp-if-needed exp)))
		   ((array `(zcptr>array ,cons)))
		   ((index `(zcptr>index ,cons))))
	      (values let-clauses array index (car exp)))))
	 ((eq (car exp) 'nlet)
	  (nlet ((let-clauses array index conses-p
		  (zcprim>analyze-pointer-exp-f (caddr exp) type must-bind)))
	    (values (zcprim>subordinate-nlet-clauses (cadr exp) let-clauses)
		    array index conses-p)))
	 (((let-clauses array index arefs-p (zcprim>analyze-aref-exp exp type))
	   arefs-p)
	  (nlet ((array array-lets (zcprim>make-temp-if-needed array))
		 (index index+1 index-lets (zcprim>+-0-and-1 index)))
	    (values (zcprim>subordinate-nlet-clauses
		      let-clauses (append array-lets index-lets))
		    `(,(car exp) ,array ,index)
		    `(,(car exp) ,array ,index+1)
		    t)))
	 ((not must-bind)
	  (values nil `(zcptr>array ,exp) `(zcptr>index ,exp) nil))
	 (t
	   (let ((temp-var (zcprim>gen-var)))
	      (values `((,temp-var ,exp)) `(zcptr>array ,temp-var)
		      `(zcptr>index ,temp-var) nil)))))

(defun zcprim>analyze-aref-exp (exp type)
  "Analyzes an expression to see if it is doing aref (pointer dereference).  In
   any case, it returns four values: a list of let-clauses; an expression for the
   array of the pointer; an expression for the index of the pointer; and T if the
   expression is, in fact, consing.  The expressions are valid only in the context 
   of the bindings set up by the let-clauses.  MUST-BIND should be NIL only in
   those cases where only one of the array and index expressions will be used.
   RESTRICTION: if EXP is an NLET-expression, it must have only one form in
   its body!"
  (cond ((nlistp exp) (values nil nil nil nil))
	((memq (car exp) '(zcptr>aref zcptr>aref-s8b zcptr>aref-s16b aref))
	 (values nil (second exp) (third exp) (car exp)))
	((eq (car exp) 'nlet)
	 (nlet ((let-clauses array index arefs-p
			     (zcprim>analyze-aref-exp (caddr exp) type)))
	   (values (zcprim>subordinate-nlet-clauses (cadr exp) let-clauses)
		   array index arefs-p)))
	(t (values nil nil nil nil))))

; This is still occasionally useful.
(defun zcprim>canonicalize-if-needed (exp type &optional req-type)
  "Wraps a form around EXP to canonicalize it, if TYPE is an array type."
  (cond ((and (stringp exp) (not (zctype>array-p req-type)))
	 `(quote ,(cons-in-area exp 0 zc-permanent-area)))
	((and (zctype>array-p type) (not (zctype>array-p req-type)))
	 (nlet ((let-clauses array index (zcprim>analyze-pointer-exp exp type)))
	   (zcprim>let-form let-clauses `(zcptr>cons ,array ,index))))
	((and (zctype>zero-p type) (zctype>arith-pointer-p req-type))
	 (zcprim>null-pointer))
	(t exp)))

(defun zcprim>scale-ptr-offset (offset type env)
  "If a /"flat/" storage representation is in use, pointer offsets have to be scaled
   by the size of the object pointed to."
  (if (not (zctype>pointer-p type))
      offset
    (zcprim>* offset (zctype>sizeof-in-scale (zctype>pointer-deref-type type) env))))

(defun zcprim>unscale-ptr-difference (difference type env)
  "If a /"flat/" storage representation is in use, pointer subtraction has to divide
   the result by the size of the object pointed to."
  (if (not (zctype>pointer-p type))
      difference
    (zcprim>// difference
	       (zctype>sizeof-in-scale (zctype>pointer-deref-type type) env))))

(defun zcprim>+ (arg1 &optional arg2)
  (cond ((null arg2) arg1)
	((and (numberp arg1) (numberp arg2))
	 (+ arg1 arg2))
	((eql arg1 0) arg2)
	((eql arg2 0) arg1)
	((and (numberp arg1)		     ; re-associate if it helps.
	      (listp arg2) (eq (car arg2) '+) (numberp (caddr arg2)))
	 `(+ ,(cadr arg2) ,(+ arg1 (caddr arg2))))
	((and (numberp arg2)		     ; and the other way.
	      (listp arg1) (eq (car arg1) '+) (numberp (caddr arg1)))
	 `(+ ,(cadr arg1) ,(+ arg2 (caddr arg1))))
	;; Keep constants in the caddr for the above optimizations.
	((numberp arg1) `(+ ,arg2 ,arg1))
	(t `(+ ,arg1 ,arg2))))

(defun zcprim>+-0-and-1 (exp)
  "Returns three values: expressions for EXP and (+ EXP 1), and let-clauses within
   which those are valid.  This performs a couple of optimizations useful when
   loading pointers out of structures."
  (cond ((numberp exp) (values exp (1+ exp) nil))
	((symbolp exp)			     ; OK to evaluate symbols twice.
	 (values exp `(+ ,exp 1) nil))
	((and (eq (car exp) '+)		     ; (+ x 2) => (+ x 2) (+ x 3)
	      (symbolp (cadr exp)) (numberp (caddr exp)))
	 (values exp `(+ ,(cadr exp) ,(1+ (caddr exp))) nil))
	(t
	 (let ((temp (zcprim>gen-var)))
	   (values temp `(+ ,temp 1) `((,temp ,exp)))))))

(defun increment-optimization (form)
  (or (and (= (length form) 3)
	   (or (and (or (and (eq (car form) '+) (eql (caddr form) 1))
			(and (eq (car form) '-) (eql (caddr form) -1)))
		    `(1+ ,(cadr form)))
	       (and (eq (car form) '+) (eql (cadr form) 1)
		    `(1+ ,(caddr form)))
	       (and (or (and (eq (car form) '+) (eql (caddr form) -1))
			(and (eq (car form) '-) (eql (caddr form) 1)))
		    `(1- ,(cadr form)))
	       (and (eq (car form) '+) (eql (cadr form) -1)
		    `(1- ,(caddr form)))))
      form))

#+Symbolics (progn (compiler:add-optimizer + increment-optimization 1+ 1-)
		   (compiler:add-optimizer - increment-optimization 1- 1+))
; compiler:add-optimizer is advertised to do the most recently added optimizations
; first, but it seems to do them last, so I get around it this way.
#+MIT (progn (push 'increment-optimization (get '+ 'compiler:optimizers))
	     (push 'increment-optimization (get '- 'compiler:optimizers)))
; The TI system does most of this already, but misses a couple of cases.
#+TI (progn (compiler:add-optimizer + increment-optimization 1+ 1-)
	    (compiler:add-optimizer - increment-optimization 1- 1+))

(defun zcprim>- (arg1 &optional arg2)
  (if (null arg2)
      (if (numberp arg1) (- arg1) `(- ,arg1))
    (zcprim>+ arg1 (zcprim>- arg2))))

(defun zcprim>* (arg1 arg2)
  (cond ((and (numberp arg1) (numberp arg2))
	 (* arg1 arg2))
	((or (eql arg1 0) (eql arg2 0)) 0)
	((eql arg1 1) arg2)
	((eql arg2 1) arg1)
	((and (numberp arg1)		     ; re-associate if it helps.
	      (listp arg2) (eq (car arg2) '*) (numberp (caddr arg2)))
	 `(* ,(cadr arg2) ,(* arg1 (caddr arg2))))
	((and (numberp arg2)		     ; and the other way.
	      (listp arg1) (eq (car arg1) '*) (numberp (caddr arg1)))
	 `(* ,(cadr arg1) ,(* arg2 (caddr arg1))))
	;; Keep constants in the caddr for the above optimizations.
	((numberp arg1) `(* ,arg2 ,arg1))
	(t `(* ,arg1 ,arg2))))

(defun zcprim>// (arg1 arg2)
  (cond ((and (numberp arg1) (numberp arg2))
	 (// arg1 arg2))
	((eql arg1 0) 0)
	((eql arg2 1) arg1)
	((eql arg2 0)
	 (zcerror "You have written a division by a constant zero."))
	(t `(// ,arg1 ,arg2))))

(defun zcprim>\ (arg1 arg2)
  (cond ((and (fixp arg1) (fixp arg2))
	 (\ arg1 arg2))
	((eql arg1 0) 0)
	(t `(\ ,arg1 ,arg2))))

(defun zcprim>ash (arg1 arg2)
  (cond ((and (fixp arg1) (fixp arg2))
	 (ash arg1 arg2))
	((eql arg1 0) 0)
	((eql arg2 0) arg1)
	(t `(ash ,arg1 ,arg2))))

(defun zcprim>logand (arg1 arg2)
  (cond ((and (fixp arg1) (fixp arg2))
	 (logand arg1 arg2))
	((eql arg1 -1) arg2)
	((eql arg2 -1) arg1)
	((or (eql arg1 0) (eql arg2 0)) 0)
	(t `(logand ,arg1 ,arg2))))

(defun zcprim>logior (arg1 arg2)
  (cond ((and (fixp arg1) (fixp arg2))
	 (logior arg1 arg2))
	((eql arg1 0) arg2)
	((eql arg2 0) arg1)
	((or (eql arg1 -1) (eql arg2 -1)) -1)
	(t `(logior ,arg1 ,arg2))))

(defun zcprim>logxor (arg1 arg2)
  (cond ((and (fixp arg1) (fixp arg2))
	 (logxor arg1 arg2))
	((eql arg1 0) arg2)
	((eql arg2 0) arg1)
	((eql arg1 -1) (zcprim>lognot arg2))
	((eql arg2 -1) (zcprim>lognot arg1)) 
	(t `(logxor ,arg1 ,arg2))))

(defun zcprim>lognot (arg)
  (if (fixp arg) (lognot arg) `(lognot ,arg)))

(defun zcprim>ldb (bytespec word)
  ;; We "know" a constant bytespec expression is a number.
  (unless (and (listp bytespec) (eq (car bytespec) 'byte))
    (ferror "ZETA-C internal error: (BYTE ...) form expected, not ~S" bytespec))
  (cond ((and (numberp (cadr bytespec)) (numberp (caddr bytespec)) (numberp word))
	 (ldb (byte (cadr bytespec) (caddr bytespec)) word))
	(t `(ldb ,bytespec ,word))))

(defun zcprim>8-bit-sign-extend (byte)
  (cond ((numberp byte)
	 (dpb byte (byte 7 0) (- (ldb (byte 1 7) byte))))
	(t (nlet ((byte let-clauses (zcprim>make-temp-if-needed byte)))
	     (zcprim>let-form let-clauses
	       `(dpb ,byte (byte 7 0) (- (ldb (byte 1 7) ,byte))))))))

(defun zcprim>16-bit-sign-extend (halfword)
  (cond ((numberp halfword)
	 (dpb halfword (byte 15 0) (- (ldb (byte 1 15) halfword))))
	(t (nlet ((halfword let-clauses (zcprim>make-temp-if-needed halfword)))
	     (zcprim>let-form let-clauses
	       `(dpb ,halfword (byte 15 0) (- (ldb (byte 1 15) ,halfword))))))))

(defun zcprim>unsigned-< (arg1 arg2)
  (cond ((and (numberp arg1) (numberp arg2))
	 (minusp (if (minusp (logxor arg1 arg2)) arg2 (- arg1 arg2))))
	((numberp arg1)
	 (nlet ((arg2 let-clauses (zcprim>make-temp-if-needed arg2)))
	   (if (minusp arg1)
	       (zcprim>let-form let-clauses `(and (minusp ,arg2) (< ,arg1 ,arg2)))
	     (zcprim>let-form let-clauses `(or (minusp ,arg2) (< ,arg1 ,arg2))))))
	((numberp arg2)
	 (nlet ((arg1 let-clauses (zcprim>make-temp-if-needed arg1)))
	   (if (minusp arg2)
	       (zcprim>let-form let-clauses
		 `(or (not (minusp ,arg1)) (< ,arg1 ,arg2)))
	     (zcprim>let-form let-clauses
	       `(and (not (minusp ,arg1)) (< ,arg1 ,arg2))))))
	(t
	 (nlet ((arg1 arg1-lets (zcprim>make-temp-if-needed arg1))
		(arg2 arg2-lets (zcprim>make-temp-if-needed arg2)))
	   (zcprim>let-form (append arg1-lets arg2-lets)
	     `(minusp (if (minusp (logxor ,arg1 ,arg2)) ,arg2
			(- ,arg1 ,arg2))))))))

;; Thanks to Robert Cassels for this version, which seems to be adequately fast.
;; GAAK!  This subst doesn't make temps for its args in all the right cases!
;;(defsubst unsigned-< (a b)
;;  (minusp (if (minusp (logxor a b))
;;	      ;; different signs
;;	      b
;;	    ;; same signs [no overflow to bignums possible]
;;	    (- a b))))

(defun zcprim>fix (arg)
  (cond ((numberp arg) (fix arg))
	(t `(fix ,arg))))

(defun zcprim>float (arg)
  (cond ((numberp arg) (#+3600 float #-3600 small-float arg))
	((and (listp arg)
	      (memq (car arg) '(#-3600 small-float float #+3600 dfloat)))
	 `(#+3600 float #-3600 small-float ,(cadr arg)))
	(t `(#+3600 float #-3600 small-float ,arg))))

(defun zcprim>dfloat (arg)
  (cond ((numberp arg) (#+3600 dfloat #-3600 float arg))
	((and (listp arg) (eq (car arg) #+3600 'dfloat #-3600 'float))
	 arg)
	(t `(#+3600 dfloat #-3600 float ,arg))))


; ================================================================
; Type checking and result type computation routines, used by the primitives.

(defun zcprim>unary-arith-result (name type)
  "Computes the type of the result of a unary arithmetic operation.  Gives an
   error if the argument is not a number."
  (if (not (zctype>number-p type))
      (zcerror "Wrong argument type to ~A: ~A" name type)
    type))

(defun zcprim>binary-arith-result (name type1 type2)
  "Computes the type of the result of a binary arithmetic operation.  Gives an
   error if the arguments are not the same kinds of numbers."
  (if (not (and (zctype>number-p type1) (zctype>number-p type2)
		(equal type1 type2)))
      (zcerror "Wrong argument type to ~A: ~A or ~A" name type1 type2)
    type1))

(defun zcprim>unary-int-arith-result (name type)
  "Computes the type of the result of a unary arithmetic operation whose domain
   is restricted to integers.  Gives an error if the argument is not an integer."
  (if (not (zctype>integer-p type))
      (zcerror "Wrong argument type to ~A: ~A" name type)
    type))

(defun zcprim>binary-int-arith-result (name type1 type2)
  "Computes the type of the result of a binary arithmetic operation whose domain
   is restricted to integers.  Gives an error if the arguments are not integers."
  (if (not (and (zctype>integer-p type1) (zctype>integer-p type2)
		(equal type1 type2)))
      (zcerror "Wrong argument type to ~A: ~A or ~A" name type1 type2)
    type1))


; ================================================================
; Miscellaneous functions used by the above.

; Called by the zcmac> level to translate variable references.
(defun zcprim>variable-reference (var env context)
  (let ((ident (zcenv>identifier var env)))
    (cond ((null ident)
	   (if (memq ':funcall (car context))
	       (values var (zcprim>undeclared-fun-type var context))
	     (if (not (memq *c-package* (package-use-list (symbol-package var))))
		 (values var (zctype>lispval))
	       (ferror "Undeclared variable: ~S" var))))
	  ((eq (car ident) 'enum-constant)
	   (values (caddr ident) (cadr ident)))
	  ((eq (car ident) 'static-alternate-name)
	   (zcprim>variable-reference (cdr ident) env context))
	  ((zctype>function-p (cdr ident))    ; function-valued symbols.
	   (if (memq ':funcall (car context))
	       (values var (cdr ident))
	     (values `(zcprim>function ,var) (zctype>pointer-to (cdr ident)))))
	  ((eq (zcenv>get var 'arghack env) ':restarg)
	   (values `(zcptr>aref ,var 0) (cdr ident)))
	  (t (values var (cdr ident))))))

(defun zcprim>undeclared-fun-type (var context)
  (zctype>function-returning
    (if (memq ':statement (cadr context))
	(zctype>void)
      (if (not (or (eq *c-package* (symbol-package var))
		   (memq *c-package* (package-use-list (symbol-package var)))))
	  (zctype>lispval)
	(zctype>int)))))

; We want to be able to tell this from plain old QUOTE.
(defmacro zcprim>function (foo)
  `',foo)

(defun zcprim>pointer-var-pair (var)
  "Returns, as two values, the names of the variables that hold the array and index
   for this pointer variable."
  (let ((cached-names (get var 'cached-pointer-var-pair))
	(uppercase (string-uppercase-p var)))
    (if cached-names (values (car cached-names) (cdr cached-names))
      (let ((array-sym (intern (string-append var (if uppercase ".ARRAY" ".array"))
			       (symbol-package var)))
	    (index-sym (intern (string-append var (if uppercase ".INDEX" ".index"))
			       (symbol-package var))))
	(putprop var (cons array-sym index-sym) 'cached-pointer-var-pair)
	(values array-sym index-sym)))))

(defun zcprim>variable-address-var (var)
  "Returns the name of the variable that holds the address array for this
   variable."
  (let ((cached-name (get var 'cached-variable-address-var)))
    (or cached-name
	;; See previous function for comments on INTERN call.
	(let ((sym (intern (string-append var (if (string-uppercase-p var) ".ADDRESS"
						".address"))
			   (symbol-package var))))
	  (putprop var sym 'cached-variable-address-var)
	  sym))))

(defun string-uppercase-p (str)
  "Returns T if all the alphabetic characters in STR are uppercase, else NIL."
  (gmap (:and) #'(lambda (ch) (not (lower-case-p ch)))
	(:string str)))

; We have to handle SETJMP at this level.  Sigh...
(defun zcprim>translate-block (stmts env context &rest other-context-stuff)
  "Translates a list of statements, producing a list of forms."
  (mapcan #'(lambda (stmt)
	      (nlet ((context-frame `(:statement nil . ,other-context-stuff))
		     ((forms (zcmac>translate-exp stmt env
						  (cons context-frame context)))))
		(if (not (second context-frame))
		    forms
		  ;; A setjmp call appeared somewhere in this statement.
		  ;; (second context-frame) is stuff to prepend to the code.
		  `(,@(second context-frame)
		    . ,forms))))
	  stmts))

; Called by the zcmac> level to do usual things with context.
(defun zcprim>standard-coercions (exp type context orig-exp)
  "This function performs standard coercions based on the expression context:
   it can convert between booleans and ints, and can produce lvalues."
  (cond ((and (memq ':statement (car context))
	      type)			   ; if it has a type, it's not a stmt-list
	 (zcprim>exp-to-statement exp type))
	((memq ':lvalue (car context))
	 (values exp (zcprim>lvalue-ok-check exp type orig-exp)))
	((and (memq ':boolean (car context)) (not (zctype>boolean-p type)))
	 (values
	   (cond ((zctype>arith-pointer-p type)
		  (zcprim>pointer-compare
		    exp type (zcprim>null-pointer)
		    (zctype>null-pointer (zctype>pointer-deref-type type))
		    'eql nil))
		 ((or (zctype>array-p type) (zctype>struct-p type))
		  (zcerror "Attempt to use expression ~A, of type ~A, as a predicate"
			   orig-exp type))
		 ((zctype>lispval-p type) exp)
		 ((numberp exp) (neql exp 0))
		 ((zctype>function-pointer-p type)
		  `(neq ,exp 'null-function-pointer))
		 (t `(neql ,exp 0)))
	   (if (zctype>lispval-p type) type (zctype>boolean))))
	((and (zctype>boolean-p type)
	      (not (memq ':boolean (car context))))
	 (values (if (memq exp '(T NIL)) (if exp 1 0) `(if ,exp 1 0))
		 (zctype>int)))
	((and (zctype>function-p type) (not (memq ':funcall (car context))))
	 (values exp (zctype>pointer-to type)))
	((memq ':unary (car context)) (zcprim>standard-unary-coercions exp type))
	(t (values exp type))))

(defun zcprim>exp-to-statement (exp type)
  "Converts an expression to a statement, that is, a list of Lisp forms."
  (if (or (zctype>arith-pointer-p type) (zctype>struct-p type))
      ;; Prevent CONS D-IGNORE instructions.
      (nlet ((let-clauses array index conses-p
			  (zcprim>analyze-pointer-exp exp type)))
	(if conses-p
	    (if let-clauses
		(if (and (eq array (caar let-clauses))
			 (eq index (cadar let-clauses))
			 (null (cdr let-clauses)))
		    ;; In this case, we've called a pointer-valued function
		    ;; for effect -- we can discard the entire nlet structure.
		    (ncons (caddar let-clauses))
		  (ncons (zcprim>let-form let-clauses
			   `(zcprim>ignore ,array ,index))))
	      `((zcprim>ignore ,array ,index)))
	  (ncons exp)))
    (ncons exp)))

(defvar *known-pure-function-list*
	'(+ - * // aref zcptr>aref zcptr>aref-s8b zcptr>aref-u8b zcptr>aref-s16b
	    zcptr>cons logand logior lognot ignore zcprim>ignore))

(defmacro zcprim>ignore (&rest stuff)
  "Yeesh.  Who'd have thought I'd have to write this?"
  `(progn . ,(mapcar #'zcprim>ignore-1 stuff)))
(defun zcprim>ignore-1 (exp)
  (cond ((nlistp exp) exp)
	((memq (car exp) *known-pure-function-list*)
	 `(progn . ,(mapcar #'zcprim>ignore-1 (cdr exp))))
	(t exp)))

(defun zcprim>lvalue-ok-check (exp type orig-exp)
  "Checks to see if a (translated) expression is legally used as an lvalue.  Gives
   an error if not."
  (if (or (and (symbolp exp)
	       (not (zctype>function-p type))
	       (not (zctype>array-p type)))   ; can't assign to arrays, functions
	  (and (listp exp)
	       (or (get (car exp) 'zcprim>lvalue-form)
		   (and (eq (car exp) 'nlet)
			(zcprim>lvalue-ok-check (caddr exp) type orig-exp))
		   (and (memq (car exp) '(ldb ldb-signed))
			(zcprim>lvalue-ok-check (caddr exp) type orig-exp)))))
      type
    (zcerror "Expression ~A of type ~A cannot be used as an lvalue"
	     orig-exp type)))

(defun zcprim>standard-unary-coercions (exp type)
  "Does the standard unary coercions on an expression and its type."
  (declare (values exp type))
  (if (and (zctype>float-p type)
	   (not *suppress-auto-float-to-double*)
	   (not (zctype>double-p type)))
      (values (zcprim>dfloat exp) (zctype>double))
    (if (zctype>enum-p type) (values exp (zctype>int))
      (values exp (zctype>widen-integral type)))))

(defun zcprim>standard-binary-coercions (arg1 type1 arg2 type2)
  "Does the standard binary coercions on a pair of args and their types."
  (declare (values arg1 type1 arg2 type2))
  ;; First we do the unary coercions to each arg.
  (nlet ((arg1 type1 (zcprim>standard-unary-coercions arg1 type1))
	 (arg2 type2 (zcprim>standard-unary-coercions arg2 type2))
	 ;; Symmetry: check one way, then the other.
	 ((arg1 type1 (zcprim>standard-binary-coercions-1 arg1 type1 type2))
	  ((arg2 type2 (zcprim>standard-binary-coercions-1 arg2 type2 type1)))))
    (values arg1 type1 arg2 type2)))
(defun zcprim>standard-binary-coercions-1 (arg1 type1 type2)
  (cond ((or (not (zctype>number-p type1)) (not (zctype>number-p type2)))
	 (values arg1 type1))
	((and (zctype>double-p type2) (not (zctype>double-p type1)))
	 (values (zcprim>dfloat arg1) type2))
	((and *suppress-auto-float-to-double*
	      (zctype>float-p type2) (not (zctype>float-p type1)))
	 (values (zcprim>float arg1) type2))
	((and (not *suppress-auto-float-to-double*)
	      (zctype>float-p type2) (not (zctype>double-p type2)))
	 (zcerror "Internal error: FLOAT encountered in DOUBLE mode"))
	((zctype>unsigned-long-p type2) (values arg1 type2))
	((and (zctype>unsigned-p type1) (zctype>long-p type2))
	 (values arg1 (zctype>unsigned-long)))
	;; Have to check this one both ways.
	((and (zctype>long-p type1) (zctype>unsigned-p type2))
	 (values arg1 (zctype>unsigned-long)))
	((zctype>unsigned-p type2) (values arg1 type2))
	((zctype>long-p type2) (values arg1 type2))
	((zctype>zero-p type1) (values arg1 type2))
	(t (values arg1 type1))))

(defun zcprim>eval-constant-int (exp)
  (multiple-value-bind (trans-exp type)
      (zcmac>translate-exp exp (zcenv>global-env) nil)
    (if (or (not (fixnump trans-exp))	     ; With constant folding, we can now
	    (not (zctype>integer-p type)))   ; count on getting a fixnum
	(ferror "Expression ~A does not evaluate to an integer constant" exp)
      trans-exp)))

(defun zcprim>subordinate-nlet-clauses (outer inner)
  "Given two lists of nlet-clauses, puts them together in such a way that the
   expressions in INNER will be evaluated in an environment that includes the
   bindings of OUTER."
  (if (null inner) outer
    (let ((maxdepth (zcprim>nlet-clause-max-depth outer)))
      (append outer (ncons-n inner (if (= maxdepth 1) 1 (1- maxdepth)))))))

(defun ncons-n (x depth)
  (if ( depth 0) x (ncons-n (ncons x) (1- depth))))

(defun zcprim>nlet-clause-max-depth (clauses)
  (cond ((null clauses) 0)
	((nlistp (car clauses)) 1)
	(t (1+ (apply #'max (mapcar #'zcprim>nlet-clause-max-depth clauses))))))

(defun zcprim>make-temp-if-needed (exp)
  "Returns two values: an expression which will evaluate to the value of EXP, and a
   list of NLET clauses which must be in force for this to be true."
  (declare (values exp-or-temp nlet-clauses))
  (if (zcprim>trivial-p exp)
      (values exp nil)
    (let ((temp-var (zcprim>gen-var)))
      (values temp-var `((,temp-var ,exp))))))

(defun zcprim>trivial-p (exp)
  "Is this expression trivial to execute?"
  (or (nlistp exp)
      (and (memq (car exp) '(zcptr>array zcptr>index zcptr>flat-deref))
	   (zcprim>trivial-p (cadr exp)))
      (equal exp (zcprim>null-pointer))))		    ; For now; more later?

(defun zcprim>let-form (clauses body-form)
  "Constructs a LET form, using CLAUSES, unless CLAUSES is NIL.  Only one BODY-FORM
   is permitted, for the sake of other code which must parse the resulting LET
   form."
  (if (null clauses) body-form
    `(nlet ,clauses ,body-form)))

#-Genera
(push '(zcprim>let-form 1 1) zwei:*lisp-indent-offset-alist*)
#+Genera
(puthash 'zcprim>let-form '(1 1) zwei:*lisp-indentation-offset-hash-table*)

; This stuff is so I can tell my gensyms from the Lisp compiler's.
(defvar zcprim>*var-gensym-counter* 0)
(defvar zcprim>*label-gensym-counter* 0)

(defun zcprim>gen-var ()
  "Makes and returns a gensym for use as a temporary local variable."
  (nlet ((#+Symbolics si:*gensym-prefix #-Symbolics si:*gensym-prefix* #/V)
	 (#+Symbolics si:*gensym-counter #-Symbolics si:*gensym-counter*
	  zcprim>*var-gensym-counter*)
	 ((symbol (gensym))))
    (setq zcprim>*var-gensym-counter* #+Symbolics si:*gensym-counter
				      #-Symbolics si:*gensym-counter*)
    symbol))

(defun zcprim>gen-label ()
  "Makes and returns a gensym for use as a PROG-label."
  (nlet ((#+Symbolics si:*gensym-prefix #-Symbolics si:*gensym-prefix* #/L)
	 (#+Symbolics si:*gensym-counter #-Symbolics si:*gensym-counter*
	  zcprim>*label-gensym-counter*)
	 ((symbol (gensym))))
    (setq zcprim>*label-gensym-counter* #+Symbolics si:*gensym-counter
					#-Symbolics si:*gensym-counter*)
    symbol))

; Forms whose CARs are the following are invertible by SETF and LOCF,
; and therefore are candidates for lvalues.  We maintain our own database
; of these because of the various incompatible ways there now are to say
; how to SETF something.

(defprop zcptr>aref t zcprim>lvalue-form)
(defprop zcptr>aref-s8b t zcprim>lvalue-form)
(defprop zcptr>aref-s16b t zcprim>lvalue-form)

(defprop aref t zcprim>lvalue-form)

(defprop zcptr>load t zcprim>lvalue-form)

(defprop zcptr>flat-deref t zcprim>lvalue-form)


; End of ZCPRIM.LISP
