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

(defun zcprim>structure-assign (src dest type env)
  ;; I know this looks strange but it works iff zcprim>struct-reference does.
  (nlet ((int-array (zctype>array-of (zctype>int)))
	 ((src-lets src-array src-index (analyze-pointer-exp src int-array))
	  (dst-lets dst-array dst-index
	    (analyze-pointer-exp dest int-array))))
    (zcprim>let-form (zcprim>subordinate-nlet-clauses
		       (append src-lets dst-lets)
		       `((ignore (zcprim>copy-structure
				   ,src-array ,src-index ,dst-array ,dst-index
				   ,(zctype>sizeof-in-scale type env)))))
      `(zcptr>cons ,dst-array ,dst-index))))

; ================================================================
; Array and structure referencing.

(defprim c:[]
  (((arg1 type1) (arg2 type2))
   (cond ((and (zctype>arith-pointer-p type1) (zctype>integer-p type2))
	  (nlet ((offset-exp type
		  (zcprim>pointer-plus-int arg1 arg2 type1 #'zcprim>+ **env)))
	    (deref-pointer offset-exp type **env)))
	 ((and (zctype>arith-pointer-p type2) (zctype>integer-p type1))
	  (nlet ((offset-exp type
		  (zcprim>pointer-plus-int arg2 arg1 type2 #'zcprim>+ **env)))
	    (deref-pointer offset-exp type **env)))
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
     (zcprim>struct-reference "->" (deref-pointer struct-p stype **env)
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
		(deref-pointer
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
  (nlet ((temp (gen-var))
	 (size (cadr bytespec)))
    `(nlet ((,temp (ldb ,bytespec ,word)))
       (dpb ,temp (byte ,(1- size) 0) (- (ldb (byte 1 ,(1- size)) ,temp))))))

(defun zcprim>coerce-struct-pointer (ptr elt-type unpacked)
  "Given a pointer to some kind of struct, coerces it to point to ELT-TYPE."
  (nlet ((let-clauses array index
	  (analyze-pointer-exp ptr (zctype>array-of (zctype>int)))))
    (selectq (zctype>type-scale elt-type unpacked)
      (:Q ptr)
      (:16B (zcprim>let-form let-clauses
	      `(zcptr>cons (zcprim>art-16b-slot ,array) ,(zcprim>* index 2))))
      (:8B (zcprim>let-form let-clauses
	     `(zcptr>cons (zcprim>art-8b-slot ,array) ,(zcprim>* index 4)))))))

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

(defprim c:|setjmp|
  (((jb-exp jb-type))
   (when (not (zctype>lispval-p jb-type))
     (zcerror "Wrong type argument, /"~A/", to setjmp"
	      (zclstn>type-string jb-type)))
   (when (second (assq ':statement **context))
     (zcerror "ZETA-C restriction: only one SETJMP call allowed in a statement"))
   (let ((label (intern (gen-label))))
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
	 (((*expanding-defunc+* func)))
	 (*defun-specials* nil)
	 (*defun-static-inits* nil)
	 (*defun-cleanup-forms* nil)
	 (*defun-toplevel-forms* nil)
	 (*defun-function-call-info* nil)
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
  (let ((type (var-type (car params) env)))
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
       (let ((ret-temp (gen-var)))
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
                      ;; (unspecial ,@(rem-if (lambda (x) (memq x '(&optional &rest))) lambda-list)) ;; cl has no equivalent - is this needed?
                      (special ,@*defun-specials*)
                      (special ,@(mapcar #'car zcprim>*defun-static-inits*)))
		     ,body)))))))
    (values lambda-list body)))

(defun zcprim>massage-params (params env)
  "Returns a list of let-clauses that do any necessary coercions on parameters."
  (zcprim>massage-params-1 params env nil))
(defun zcprim>massage-params-1 (params env clauses)
  (if (null params) (nreverse clauses)
    (nlet ((param (car params))
	   ((type (var-type param env))))
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

(defun zcprim>bind-temp-displaced-array (var displaced-to length art id nss)
  "Returns an expression that binds VAR to a temporary array which is displaced to
   DISPLACED-TO, of length LENGTH (the latter two are expressions).  ART is the
   array-type symbol; ID is the id-list of the array (leader 0); NSS is the
   named-structure-symbol.  Second value is a list of cleanup-forms."
  (let ((freelist-var (gen-var)))
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

; ================================================================
; External declarations, system type definition, and misc.

; Yeesh, this is huge.  But there really are all these cases.
(defun zcprim>cast (exp exp-type type **env)
  (cond ;; Special case for null pointers.
    ((and (zctype>arith-pointer-p type) (zctype>zero-p exp-type))
     (values (zcprim>null-pointer)
	     (zctype>null-pointer (zctype>pointer-deref-type type))))
    ((and (zctype>word-p exp-type) (zctype>arith-pointer-p type))
     ;; An int (or lispval) to a pointer -- see if the int already holds a cons.
     (nlet ((exp let-clauses (zcprim>make-temp-if-needed exp))
	    (array (gen-var))
	    (index (gen-var))
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
			 (analyze-pointer-exp exp exp-type)))
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
	 (nlet ((let-clauses array index (analyze-pointer-exp exp exp-type))
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
	     (analyze-pointer-exp exp exp-type :must-bind nil)))
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

; ================================================================
; Miscellaneous functions used by the above.

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





