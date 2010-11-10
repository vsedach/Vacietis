; This file contains the type checking and variable type recording code
; for Zeta-C.

; ================================================================
; "Environment" abstraction maintains properties of symbols: their types,
; typedef definitions if any, #defined value if any, or structure or
; union definition if any.

(defvar *source-location* nil
  "An object identifying the file and line number on which the declaration or
   definition being processed began; or NIL if it was entered interactively.")

; These communicate with cparse:LWhere() in zcparse.lisp.
(defsubst zcenv>source-loc-file ()
  (first *source-location*))

(defsubst zcenv>source-loc-line ()
  (second *source-location*))

(defsubst zcenv>source-editor-p ()
  (third *source-location*))

(defsubst zcenv>source-predefined-loc ()
  '(:predefined 0))

; In C, all identifiers used in a file must have been declared in that file
; (or in files it includes).  As a convenience to the user, we relax this
; restriction in most cases, but print a warning when we do so (see
; ZCENV>GLOBAL-GET below).
(defvar zcenv>*attempt-substitution* t
  "If a declaration is not found in this file, do we attempt to find one in
   another file?  (T by default)")

; Under certain circumstances (e.g. when we're only looking for parameter types),
; we don't want to print the warning.
(defvar zcenv>*substitute-quietly* nil
  "Don't warn when using a declaration from another file.  (NIL by default)")

; When the user is editing and incrementally compiling, it's possible for her to
; get her declarations into an inconsistent state -- for instance by deleting a
; declaration -- that will not be detected immediately.  So we call this whenever
; compiling an entire file, or when loading an object file, to reset things.
(defun zcenv>clear-file-declarations (file)
  "Clears all global declarations for the specified file.  FILE should be a file
   symbol, that is, a filename, in lowercase, interned in the appropriate package."
  (mapatoms
    #'(lambda (sym)
	(do ((plist (plist sym) (cddr plist)))
	    ((null plist))
	  (let ((prop (car plist)))
	    (when (memq prop '(identifier definition struct//union
			       enum enum-constant typedef /#define))
	      (if (not (and (listp (cadr plist)) (listp (caadr plist))
			    (symbolp (caaadr plist))
			    (or (c-package-p (symbol-package (caaadr plist)))
				(eq (caaadr plist) ':predefined))))
		  (setf (cadr plist) nil)  ; Old-style property.
		(setf (cadr plist)
		      (del-if #'(lambda (frame)
				  (and (listp frame) (eq (car frame) file)))
			      (cadr plist))))))))
    (symbol-package file) t))      ; T => do C: also.

;                           --- THE ENVIRONMENT ---
; Bindings in the global environment are made on the property lists of symbols.
; Thus, the global environment itself is represented simply as NIL.  (Other code
; really shouldn't count on this, of course, but I think it does -- check
; especially the code that initializes the leaders of arrays.)  The binding-class
; (see below) is used as the property indicator, and the property value is a list
; of the form ((file line value) ...) listing the bindings by file and line number
; (since a variable can be declared different ways in different places, depending
; on what kind of thing it is: a variable, a struct/union or enum tag, a macro).
; For an identifier declared interactively, in the C listener, the file and line
; will be NIL.
;
; A local environment looks like (ZCENV . frames), where each frame (which
; represents one environment contour) looks like
; ((var (binding-class . value) ...) ...)
; i.e., an alist mapping symbols to alists mapping binding-classes to values.
;
; The various binding-classes are (these symbols are in ZETA-C:):
;   IDENTIFIER      The symbol is a variable or function name.  The value listed
;                   under this binding-class itself has the structure
;                   (type . value), where the type is one of:
;                     TYPE    The value is the type of the identifier.
;                     STATIC-ALTERNATE-NAME    The identifier is declared static;
;                       the "value" here is its actual name (see the User's Guide).
;                     ENUM-CONSTANT   The identifier is an enumeration constant;
;                       the value given here is its value.
;   DEFINITION      (Only appears as property, i.e., in global env.)  This is the
;                   "defining declaration" of this variable or function.  The
;                   value here is its type.
;   STRUCT/UNION    The name is a struct or union tag.  The value is of the form
;                   (class . element-alist), where the class is one of
;                   :STRUCT or :UNION.  See elsewhere for the element-alist.
;   ENUM            The name is an enumeration tag.  The value is of the form
;                   (:ENUM (sym . val) ...).
;   ENUM-CONSTANT   The name is an enumeration constant.  The value is of the form
;                   ((:ENUM . tag) val).
;   TYPEDEF         The value is the type for which the name is a synonym.
;   ARGHACK         (Only appears in local frame.)  The variable (which must have
;                   been a parameter to the current function) is declared as a
;                   restarg or optarg.  The value is :RESTARG or :OPTARG.

(defun zcenv>create-env (parent)
  "The parent is another environment or NIL (global)."
  (list* 'zcenv nil (cdr parent)))

(defun zcenv>parent (env)
  "The parent of ENV.  The global env is its own parent."
  (and (cddr env) (cons 'zcenv (cddr env))))

(defun zcenv>set (var val binding-class env &optional primary-binding)
  "Creates or alters a secondary binding.  Always shadows, i.e., always uses
   lowest env frame.  Don't call this outside zcenv>.  If the binding is made
   in the global environment, returns a putprop form that accomplishes it
   (for fasdumping)."
  (if env
      (zcenv>local-set var val binding-class (cdr env))
    (let ((file (zcenv>source-loc-file))
	  (line (zcenv>source-loc-line)))
      (zcenv>global-set var val binding-class file line primary-binding)
      `(zcenv>global-set ',var ',val ',binding-class
			 ',file ',line ',primary-binding))))

(defun zcenv>global-set (var val binding-class file line &optional primary-binding)
  "Creates or alters a secondary binding in the global environment.  If
   PRIMARY-BINDING is T, this binding is made at the head of the frame list."
  (nlet ((alist (get var binding-class))
	 ((frame (assq file alist))))
    (if (null frame)
	(let ((frame (list file line val)))
	  (if (or primary-binding (null alist))
	      (putprop var (cons frame alist) binding-class)
	    (rplacd alist (cons frame (cdr alist))))
	  (when file
	    (zcenv>check-redeclaration var val binding-class file line frame)))
      (when file
	(zcenv>check-redeclaration var val binding-class file line frame))
      (when (or (null (second frame)) (< line (second frame)))
	(setf (second frame) line))
      (setf (third frame) val)
      (when primary-binding
	;; Splice this frame to the front of the alist.
	(do ((al alist (cdr al)))
	    ((null al))
	  (when (eq frame (cadr al))
	    (let ((fcons (cdr al)))
	      (rplacd al (cdr fcons))
	      (rplacd fcons alist)
	      (putprop var fcons binding-class))))))))

(defun zcenv>local-set (var val binding-class frames)
  "Sets the BINDING-CLASS binding of VAR to VAL in the topmost frame in FRAMES."
  (let ((alistpr (assq var (car frames))))
    (if alistpr
	(let ((oldvalpr (assq binding-class (cdr alistpr))))
	  (if oldvalpr
	      (setf (cdr oldvalpr) val)
	    (push (cons binding-class val) (cdr alistpr))))
	(push (list var (cons binding-class val)) (car frames)))
    nil))

(defun zcenv>check-redeclaration (var val binding-class file line bindframe)
  "Checks for various kinds of redeclarations, and warns when appropriate."
  ;; This grossly violates abstraction boundaries, but I'll fix it later.
  (selectq binding-class
    (identifier
     ;; If the value is actually changing, reset the warnings memory.
     (when (not (zcenv>identifier-equal val (third bindframe)))
       (setf (cdddr bindframe) nil))
     ;; Walk down the alist, looking for possible conflicting declarations.
     (dolist (frame (get var binding-class))
       (when (and ;; If we're not on the same line of the same file ...
	          (not (and (eq file (first frame)) (= line (second frame))))
		  ;; ... and the value being bound is not equal ...
		  (not (zcenv>identifier-equal val (third frame)))
		  ;; ... and we haven't warned about this conflict before ...
		  (not (mem #'zcenv>identifier-equal
			    (third frame) (cdddr bindframe)))
		  ;;(Exception: for static alternate names, no cross-file warnings)
		  (or (and (neq (car val) 'static-alternate-name)
			   (neq (car (third frame)) 'static-alternate-name))
		      (eq file (first frame))))    ; ... then print a warning.
	 (zcwarn "Declaration of /"~A/" as ~(~A~) /"~A/" on line ~D of ~A;~%  ~
		  was declared ~(~A~) /"~A/" ~:[interactively~;on line ~D of ~A~]"
		 var (car val)
		 (if (neq (car val) 'type) (cdr val) (zclstn>type-string (cdr val)))
		 line file (car (third frame))
		 (if (neq (car (third frame)) 'type) (cdr (third frame))
		   (zclstn>type-string (cdr (third frame))))
		 (first frame) (second frame) (first frame))
	 ;; Remember that we warned about this one.
	 (push (third frame) (cdddr bindframe)))))
    (definition
     (dolist (frame (get var binding-class))
       (when ;; When and we're either in a different file or more than 10
	     ;; lines farther down in the same file (10 is a fudge factor
	     ;; for insertions while editing), then warn.
	     (or (neq file (first frame))
		 (> line (+ 10 (second frame))))
	 (zcwarn "A defining declaration of /"~A/" appears on line ~D of ~A;~%  ~
		  one was previously seen on line ~D of ~A"
		 var line file (second frame) (first frame)))))
    (/#define)                               ; Can be redefined arbitrarily.
    (otherwise                     ; For struct/union, enum, enum-constant, typedef
     (dolist (frame (get var binding-class))
       (when ;; If we're on a different line of the same file, warn.
	     (and (eq file (first frame)) ( line (second frame)))
	 (zcwarn "Redeclaration of ~(~A~) /"~A/" on line ~D of ~A;~%  ~
		  was previously declared on line ~D"
		 binding-class var line file (second frame)))))))

(defun zcenv>identifier-equal (val1 val2)
  "Compares two values of binding-class IDENTIFIER for equality."
  (if (eq (car val1) 'type)
      (and (eq (car val2) 'type)
	   (zctype>equal (cdr val1) (cdr val2)))
    (equal val1 val2)))

(defun zcenv>local-get (var binding-class frame)
  (cdr (assq binding-class (cdr (assq var frame)))))

(defun zcenv>get (var binding-class env &optional check-previous-only)
  "Retrieves a secondary binding.  Second value is environment depth
   at which the binding was found (0 = global, more positive = deeper).
   Don't call this outside zcenv>.  CHECK-PREVIOUS-ONLY means we're looking to
   see if there is a previous declaration, but we won't actually use the
   value; it looks only in the current frame."
  (if (and env check-previous-only)
      (values (zcenv>local-get var binding-class (cadr env)) (length (cdr env)))
    ;; Iterate down the frames of the env till we find a value.
    (do ((e (cdr env) (cdr e)))
	((null e)
	 (values (zcenv>global-get var binding-class check-previous-only) 0))
      (let ((binding (zcenv>local-get var binding-class (car e))))
	(when binding (return (values binding (length e))))))))

(defun zcenv>global-get (var binding-class check-previous-only)
  "Retrieves a secondary binding in the global environment."
  (nlet ((file (zcenv>source-loc-file))
	 (line (zcenv>source-loc-line))
	 (alist (get var binding-class))
	 ;; If FILE is null, we arbitrarily take the first frame.
	 ((frame (or (assq file alist) (and (null file) (car alist))
		     (assq ':predefined alist)))
	  ((value (third frame))))
	 ((firstval (third (car alist)))))
    (if (and frame (or (null file) (null (second frame)) ( line (second frame))
		       (zcenv>source-editor-p)))
	(progn
	  (when (and file (second frame) (< line (second frame))
		     ;; Don't complain about undeclared int() or void().
		     (zcenv>boring-thing value binding-class)
		     (not zcenv>*substitute-quietly*))
	    (zcwarn "The ~(~A~) of ~A is being referenced on line ~D of ~A,~
		     ~%  but appears to be declared on line ~D"
		    (if (eq binding-class 'identifier) (car value) binding-class)
		    var line file (second frame)))
	  value)
      (if (or (null alist) (not zcenv>*attempt-substitution*) check-previous-only
	      ;; Don't substitute static alternate names!  Defeats whole purpose!
	      (and (eq binding-class 'identifier)
		   (eq (car firstval) 'static-alternate-name)))
	  nil	; No substitution.
	(unless (or zcenv>*substitute-quietly*
		    ;; Don't complain about undeclared int() or void().
		    (zcenv>boring-thing firstval binding-class))
	  (zcwarn
	    "The ~(~A~) of ~A, referenced on line ~D of ~A, has not been declared~
	     ~%  in this file.  Using value /"~A/", declared on line ~D of ~A."
	    (if (eq binding-class 'identifier) (car firstval) binding-class)
	    var line file
	    (if (eq binding-class 'identifier)
		(if (eq (car firstval) 'type) (zclstn>type-string (cdr firstval))
		  (cdr firstval))
	      firstval)
	    (second (car alist)) (first (car alist))))
	(third (car alist))))))

(defun zcenv>boring-thing (value binding-class)
  "Is VALUE not worth giving a substitution warning about?"
  (if (eq binding-class 'identifier)
      (and (listp value) (eq (car value) 'type)
	   (zctype>boring-function-type-p (cdr value)))
    (zctype>boring-function-type-p value)))

(defun zcenv>declare (var val binding-class env &optional primary-binding)
  "Creates a secondary binding.  Don't call this outside zcenv>."
  (zcenv>set var val binding-class env primary-binding))

; The following are public.

(defsubst zcenv>global-env ()
  "Returns the global environment."
  nil)

(defsubst zcenv>global-env-p (env)
  "Is ENV the global environment?"
  (null env))

; Bug in 98: the second argument cannot be named "type", because that confuses
; the compiler about the status of the expression "(quote type)".
(defun zcenv>declare-type (var var-type env &optional primary-binding)
  "Declares the type of a variable."
  (zcenv>declare var (cons 'type var-type) 'identifier env primary-binding))

(defun zcenv>type (var env)
  "Retrieves the type of a declared variable, or NIL.  Second value is the
   environment depth at which the binding was found."
  (nlet ((ident depth (zcenv>get var 'identifier env)))
    (values (and ident
		 (selectq (car ident)
		   (type (cdr ident))
		   (enum-constant (cadr ident))
		   (static-alternate-name (zcenv>type (cdr ident) env))))
	    depth)))

(defun zcenv>identifier (var env)
  "Retrieves information on identifier VAR.  This will be of one of the following
   forms: (TYPE . <type>), (ENUM-CONSTANT <type> <value>), or
   (STATIC-ALTERNATE-NAME . <name>).  Second value is the environment depth at
   which the binding was found."
  (zcenv>get var 'identifier env))

;; I don't remember quite why I thought we shouldn't look in other files.
;; This feature? is disabled anyway because ZCPRIM>VARIABLE-REFERENCE doesn't
;; call this.  If you want the former, fix the latter.
(defun zcenv>function-type (name env)
  "Retrieves the type of a function from its name, or NIL."
  ;; Don't go to another file; we'll just assume :INT.
  (let ((zcenv>*attempt-substitution* nil))
    (zcenv>type name env)))

(defsubst zcenv>declare-definition (var var-type env)
  "Declares the defining declaration of a variable."
  (zcenv>declare var var-type 'definition env))

(defsubst zcenv>definition (var env)
  "Retrieves the type of VAR given by its defining declaration."
  (zcenv>get var 'definition env))

(defsubst zcenv>declare-struct//union (tag type env)
  "Declares a struct or union tag."
  (zcenv>set tag type 'struct//union env))

(defun zcenv>struct//union-type (tag env &optional check-previous-only)
  "Retrieves the full type of a declared struct or union tag, or NIL
   if the tag is not defined.  CHECK-PREVIOUS-ONLY means we're looking to
   see if there is a previous declaration, but we won't actually use the
   value."
  (zcenv>get tag 'struct//union env check-previous-only))

(defun zcenv>declare-enum (tag tag-type elt-alist elt-type env)
  "Declares an enumeration type, as well as declaring all the enumeration constants
   it specifies.  TAG can be NIL."
  (dolist (pair elt-alist)
    (zcenv>set (car pair) (list 'enum-constant elt-type (cdr pair))
	       'identifier env))
  (and tag (zcenv>set tag tag-type 'enum env)))

(defun zcenv>enum-type (tag env &optional local-p)
  "Retrieves the full type of a declared enumeration tag, or NIL
   if the tag is not defined."
  (zcenv>get tag 'enum env local-p))

(defun zcenv>enum-constant-value (name env)
  "Returns the value of the enumeration constant, or NIL.  Second value is the
   environment depth at which the binding was found."
  (let ((ident (zcenv>get name 'identifier env)))
    (and ident (eq (car ident) 'enum-constant) (third ident))))

(defsubst zcenv>typedef (name type env)
  "Defines a new type name."
  (zcenv>declare name type 'typedef env))

(defun zcenv>typedef-type (name env)
  "Retrieves the full type of a defined type name, or NIL if the name
   is not typedef'ed."
  (zcenv>get name 'typedef env))

(defun zcenv>annotate (var kind note env)
  "Annotates a variable VAR with note NOTE, of kind KIND.  Subsequent calls of the
   form (ZCENV>ANNOTATION var kind env) will return NOTE.  Only local variables may
   be annotated."
  (do ((frames (cdr env) (cdr frames)))
      ((null frames)
       (ferror "Internal error: Attempt to annotate variable ~A, which does not~@
		appear in environment ~A" var env))
    (when (assq var (car frames))
      (zcenv>local-set var note kind frames)
      (return nil))))

(defsubst zcenv>annotation (var kind env)
  "Retrieves an annotation made with ZCENV>ANNOTATE, or NIL if none of this KIND
   has been made on VAR."
  (zcenv>get var kind env))

(defun zcenv>declare-static (name altname env)
  "Declares the alternate name for a static variable."
  (zcenv>declare name (cons 'static-alternate-name altname) 'identifier env))

(defun zcenv>lookup-static (name env)
  (let ((ident (zcenv>get name 'identifier env)))
    (and ident (eq (car ident) 'static-alternate-name) (cdr ident))))

; For use by the lexer.  This is similar to the way the other kinds of bindings
; work, but not identical: it keeps *all* #defines and #undefs in a file, so that
; editor compilations have a chance of finding the right one.
; Commented-out lines are part of a change I started (to keep the file and line
; of the most recent definition), but which won't work now because of line number
; bogosity (all lines in a section appear to start on the same line).  When that is
; fixed, this can be.
(defun zcenv>#define (sym defn *source-location*)
  "#define a symbol."
  (nlet ((file (zcenv>source-loc-file))
	 ;; From the C listener, we always use line 0.
	 ((line (if file (zcenv>source-loc-line) 0)))
	 (defn-prop (get sym '/#define)))
    ;; Back compatibility, both with very old version...
    (when (and defn-prop (or (not (listp (car defn-prop)))
			     (not (fixp (cadar defn-prop)))))
      (format error-output "~&~S had an obsolete #DEFINE property" sym)
      (setq defn-prop nil))
;    ;; ... and with more recent.  (car defn-prop) is a list (<file> <line>) that
;    ;; tells where the symbol was most recently #defined.
;    (when (or (null defn-prop) (not (null (cddar defn-prop))))
;     (setq defn-prop (cons (list nil 0) defn-prop)))
    (setf (get sym '/#define) defn-prop)
;    (do ((insert-point defn-prop (cdr insert-point)))
    ;; We keep the definitions sorted first by package, then by file, then by line.
    (do ((insert-point (locf (get sym '/#define)) (cdr insert-point)))
	((or (null (cdr insert-point))
	     (string-lessp (si:pkg-name (symbol-package file))
			   (si:pkg-name (symbol-package
					  (first (cadr insert-point)))))
	     (string-lessp file (first (cadr insert-point)))
	     (and (eq file (first (cadr insert-point)))
		  ( line (second (cadr insert-point)))))
	 (if (and (cdr insert-point)
		  (eq file (first (cadr insert-point)))
		  (= line (second (cadr insert-point))))
	     (setf (third (cadr insert-point)) defn)
	   (rplacd insert-point (cons (list file line defn) (cdr insert-point))))
	 `(zcenv>#define ',sym ',defn ',*source-location*)))))

;; This is so complicated because we need to get the correct definition for the
;; file and line we're on.
(defun zcenv>#definition (sym *source-location*)
  "Look up the #definition of a symbol."
  (nlet ((alist (get sym '/#define))
	 (file (zcenv>source-loc-file))
	 ((line (if file (zcenv>source-loc-line) 0))))
    (if (null file) (zcenv>#defn-menu sym file)
      (if (null alist) nil
	(do ((alist alist (cdr alist))
	     (prevframe (car alist) (car alist)))
	    ((or (null alist)
		 (alphalessp (si:pkg-name (symbol-package file))
			     (si:pkg-name (symbol-package (first (car alist)))))
		 (alphalessp file (first (car alist)))
		 (and (eq file (first (car alist)))
		      (< line (second (car alist)))))
	     (cond ((and (neq file (first prevframe))
			 (neq file (first (car alist))))
		    ;; No entry for this file at all.
		    nil)
		   ((or (neq file (first prevframe))
			(and (null alist) (eq file (first prevframe))
			     (< line (second prevframe))))
		    ;; First entry for this file has higher line# than current.
		    ;; Obviously this can only happen from the editor.
		    (let ((frame (if (null alist) prevframe (car alist))))
		      (and
			(y-or-n-p
			  (format nil "/"~A/", which had been first #defined on line ~D of ~A,~@
				       is being referenced on line ~D; allow the reference? "
				  sym (second frame) file line))
			(third frame))))
		   (t (third prevframe)))))))))

(defvar zcenv>#defn-menu-item-list nil)

(defresource zcenv>#defn-menu-menu ()
  :constructor (tv:make-window 'tv:dynamic-pop-up-menu
			       :item-list-pointer 'zcenv>#defn-menu-item-list))

;; When we use a macro in the C listener, and the macro has more than one
;; definition, there's no way to know which one the user means.  So we give her
;; a menu.
(defun zcenv>#defn-menu (sym file)
  (nlet ((alist (get sym '/#define))
	 ((alist (if (null file)        ; It is always null, as things stand.
		     (subset #'(lambda (frame)
				 (or (null (first frame))
				     (eq package (symbol-package (first frame)))))
			     alist)
		   (subset #'(lambda (frame) (eq file (first frame)))
			   alist)))
	  ((alist (subset #'(lambda (frame) (third frame)) alist)))))
    ;; If there are only one or no choices, don't bother after all.
    (if (or (null alist)
	    (gmap (:and) #'(lambda (frame)
			     (equal (third frame) (third (car alist))))
		  (:list (cdr alist))))
	(and alist
	     (or (null file)
		 (eq (symbol-package file) (symbol-package (first (car alist)))))
	     (third (car alist)))
      (nlet ((menu (allocate-resource 'zcenv>#defn-menu-menu))
	     (doc-string
	       (format nil
		       "Macro ~A has several possible expansions at this ~
			point.  Choose one." sym))
	     ((zcenv>#defn-menu-item-list
		(mapcar #'(lambda (frame)
			    `(,(cdr (third frame)) :value ,(third frame)
			      :documentation ,doc-string)) alist))))
	(unwind-protect
	    (progn
	      (send menu :set-label (format nil "Expansions of ~A" sym))
	      (send menu :expose-near '(:mouse))
	      (send menu :choose))
	  (send menu :deactivate)
	  (deallocate-resource 'zcenv>#defn-menu-menu menu))))))


; ================================================================
; Predicates on types.

; Here are the internal type representations used by ZETA-C.
;  :INT, :LONG                A signed integer (fixnum or bignum).
;  :UNSIGNED, :UNSIGNED-LONG  An unsigned integer (fixnum or bignum).
;  :ZERO                      The explicit constant zero (which is an element of
;                             all pointer types as well as integers).
;  :SHORT                     A signed 16-bit integer.
;  :UNSIGNED-SHORT            An unsigned 16-bit integer.
;  :CHAR                      An unsigned 8-bit integer.
;  :SIGNED-CHAR               A signed 8-bit integer.
;  :FLOAT                     A single-precision floating-point number (small-float
;                             on A-machine, single-float on 3600).
;  :DOUBLE                    A double-precision floating-point number
;                             (single-float on A-machine, double-float on 3600).
;  :LISPVAL                   An arbitrary Lisp object.
;  :VOID                      Nothing (although void * will soon work).
;  :BOOLEAN                   The internal type for predicates evaluated for effect
;                             (T or NIL).
;  (:FUNCTION type . arginfo) A function returning <type>.  <arginfo> is a list of
;                             the types of the arguments.
;  (:POINTER type {:NULL})    A pointer to <type>.  :NULL after the type indicates
;                             an explicit constant null pointer.
;  (:ARRAY type size)         An array of <type>.  <size> gives the number of
;                             elements; it can be NIL if the size is unknown.
;  (:STRUCT . frob)           A struct or packed struct (the latter is unique to
;  (:PACKED-STRUCT . frob)    ZETA-C).  The <frob> is either the tag, if one was
;                             given when the struct was declared, or the element
;                             alist, which looks like
;                             ((element offset . type) ... (:END offset)).
;  (:BITS type width offset)  A bit field (can only appear as the type of a struct
;                             or packed struct element).  <type> is either :INT or
;                             :UNSIGNED.
;  (:UNION . frob)            A union.  <frob> is either the tag, if one was given
;                             when the union was declared, or the element alist,
;                             as for a struct (except the offsets are meaningless).


(defsubst zctype>char-p (type)
  "Is this type the type of a character (unsigned)?"
  (eq type ':char))

(defsubst zctype>enum-p (type)
  "Is this the type of an enumeration?"
  (and (listp type) (eq (car type) ':enum)))

(defun zctype>integer-p (type)
  "Is this type the type of some flavor of integer?"
  (or (memq type '(:zero :char :signed-char :short :unsigned-short :int
			 :unsigned :long :unsigned-long
			 :int-or-nil))       ; latter for back compatibility.
      (zctype>enum-p type)))

(defsubst zctype>signed-char-p (type)
  "Is this type the type of a signed char?"
  (eq type ':signed-char))

(defsubst zctype>signed-short-p (type)
  "Is this type the type of a signed short?"
  (eq type ':short))

(defsubst zctype>unsigned-short-p (type)
  "Is this type the type of an unsigned short?"
  (eq type ':unsigned-short))

(defsubst zctype>shorter-than-int-p (type)
  "Does this type require explicit narrowing on store?"
  (memq type '(:char :signed-char :short :unsigned-short)))

(defsubst zctype>long-p (type)
  "Is this type the type of a long (signed)?"
  (eq type ':long))

(defsubst zctype>unsigned-p (type)
  "Is this any unsigned type?"
  (memq type '(:char :unsigned-short :unsigned :unsigned-long)))

(defsubst zctype>unsigned-long-p (type)
  "Is this type the type of an unsigned long?"
  (eq type ':unsigned-long))

(defsubst zctype>float-p (type)
  "Is this type the type of a floating-point number (of either precision)?"
  (memq type '(:float :double)))

(defsubst zctype>double-p (type)
  "Is this type the type of a double-precision floating-point number?"
  (eq type ':double))

(defsubst zctype>number-p (type)
  "Is this type the type of a number?"
  (or (zctype>integer-p type) (zctype>float-p type)))

(defsubst zctype>function-p (type)
  "Is this type the type of some function?"
  (and (listp type) (eq (car type) ':function)))

(defun zctype>boring-function-type-p (type)
  "Is this type not worth warning about?"
  (and (zctype>function-p type) (memq (cadr type) '(:int :void))))

(defsubst zctype>array-p (type)
  "Is this type the type of some array (non-canonicalized pointer)?"
  (and (listp type) (eq (car type) ':array)))

(defsubst zctype>zero-p (type)
  "Is this the special kludge type for zero?"
  (eq type ':zero))

(defsubst zctype>null-pointer-p (type)
  "Is this the special kludge type for null pointers?"
  (and (listp type) (eq (car type) ':pointer) (eq (caddr type) ':null)))

(defun zctype>pointer-p (type)
  "Is this type the type of some pointer (canonicalized or not)?"
  (and (listp type) (memq (car type) '(:pointer :array))))

(defun zctype>function-pointer-p (type)
  "Is this type some type of pointer to function?"
  (and (zctype>pointer-p type)
       (zctype>function-p (zctype>pointer-deref-type type))))

(defun zctype>arith-pointer-p (type)
  "Is this a pointer type on which arithmetic is meaningful?"
  (or (zctype>canonicalized-pointer-p type)
      (zctype>array-p type)))

(defun zctype>canonicalized-pointer-p (type)
  "Is this a canonicalized pointer type (one that is neither an array nor a
   pointer-to-function)?"
  (and (listp type) (eq (car type) ':pointer)
       (not (zctype>function-p (cadr type)))))

(defsubst zctype>boolean-p (type)
  "Is this the type of a boolean, i.e., a logical expression evaluated for control
   rather than value?"
  (eq type ':boolean))

(defsubst zctype>void-p (type)
  "Is this the void type?"
  (eq type ':void))

(defun zctype>lispval-p (type)
  "Is this the type of a Lisp value?"
  (or (eq type ':lispval) (and (listp type) (eq (car type) ':lispval))))

(defun zctype>word-p (type)
  "A machine word value: lispval, int (or shorter)."
  (or (zctype>integer-p type) (zctype>lispval-p type)))

(defsubst zctype>struct-p (type)
  "Is this the type of a structure or union?"
  (and (listp type) (memq (car type) '(:struct :packed-struct :union))))

(defun zctype>bits-p (type)
  "Is this type the /"type/" of a bit field?"
  (and (listp type) (eq (car type) ':bits)))

(defun zctype>scalar-p (type)
  "Is this the type of some scalar, i.e., something that is not implemented with
   an array?"
  (not (zctype>aggregate-p type)))

(defun zctype>aggregate-p (type)
  "Is this the type of some aggregate, i.e., something that is implemented with
   an array?"
  (or (zctype>array-p type)
      (zctype>struct-p type)))

(defun zctype>match (type1 type2 env)
  "Are these two types compatible, for assignment, function-calling, or whatever?"
  (cond ((equal type1 type2))                   ; easy case
	((or (and (zctype>number-p type1) (zctype>number-p type2))
	     (and (zctype>pointer-p type1)
		  (or (zctype>zero-p type2) (zctype>null-pointer-p type2)))
	     (and (zctype>pointer-p type2)
		  (or (zctype>zero-p type1) (zctype>null-pointer-p type1)))
	     (and (zctype>pointer-p type1) (zctype>pointer-p type2)
		  (zctype>match (zctype>pointer-deref-type type1)
				(zctype>pointer-deref-type type2) env)
		  (eq (zctype>type-scale (zctype>pointer-deref-type type1))
		      (zctype>type-scale (zctype>pointer-deref-type type2))))))
	((or (nlistp type1) (nlistp type2)) nil)
	((and (zctype>function-p type1) (zctype>function-p type2))
	 (or (and (zctype>integer-p (zctype>function-return-type type1))
		  (zctype>integer-p (zctype>function-return-type type2)))
	     (zctype>match (zctype>function-return-type type1)
			   (zctype>function-return-type type2)
			   env)))
	((and (zctype>struct-p type1) (zctype>struct-p type2)
	      (eq (zctype>struct-class type1) (zctype>struct-class type2)))
	 (and (zctype>struct-tag type1) (zctype>struct-tag type2)
	      (eq (zctype>struct-tag type1) (zctype>struct-tag type2))))
	(t nil)))

;;; Written "inside" the type abstraction.  Change this if you change the structure of
;;; a type representation!
(defun zctype>equal (type1 type2)
  "Are these two precisely the same type, distinguishing even those cases that are
   implemented identically?  (I.e., this is a much tighter individuation than used
   by ZCTYPE>MATCH; it's appropriate for LINT-like behavior.)"
  (cond ((eq type1 ':zero) (memq type2 '(:int :unsigned)))
	((eq type2 ':zero) (memq type1 '(:int :unsigned)))
	((symbolp type1) (eq type1 type2))
	((symbolp type2) nil)
	((eq (car type1) ':pointer)
	 (and (eq (car type2) ':pointer)
	      (zctype>equal (cadr type1) (cadr type2))))
	((eq (car type1) ':array)
	 (and (eq (car type2) ':array)
	      (zctype>equal (cadr type1) (cadr type2))
	      (or (null (caddr type1)) (null (caddr type2))
		  (= (caddr type1) (caddr type2)))))
	((memq (car type1) '(:struct :packed-struct :union :enum))
	 (and (eq (car type1) (car type2))
	      (eq (cdr type1) (cdr type2))))
	((eq (car type1) ':function)
	 (and (eq (car type2) ':function)
	      (zctype>equal (cadr type1) (cadr type2))
	      (or (not (cddr type1)) (not (cddr type2))
		  (gmap (:and) #'(lambda (arg1 arg2)
				   (zctype>equal (second arg1) (second arg2)))
			(:list (caddr type1))
			(:list (caddr type2))))))
	(t nil)))


; ================================================================
; Type constructors.

(defsubst zctype>char () ':char)

(defsubst zctype>signed-char () ':signed-char)

(defsubst zctype>short () ':short)

(defsubst zctype>unsigned-short () ':unsigned-short)

(defsubst zctype>int () ':int)

(defsubst zctype>long () ':long)

(defsubst zctype>unsigned () ':unsigned)

(defsubst zctype>unsigned-long () ':unsigned-long)

(defsubst zctype>float () ':float)

(defsubst zctype>double () ':double)

(defun zctype>lispval (&optional flavor)
  (if flavor (list ':lispval flavor) ':lispval))

(defsubst zctype>void () ':void)

(defsubst zctype>boolean () ':boolean)

(defsubst zctype>zero () ':zero)

(defsubst zctype>null-pointer (type)
  "Makes a type /"null-pointer-to-TYPE/"."
  (list ':pointer type ':null))

(defsubst zctype>pointer-to (type)
  "Makes a type that is a pointer to the type given."
  (list ':pointer type))

(defun zctype>array-of (type &optional length)
  "Makes a type that is an array of the type given, optionally of the given
   length."
  (when (and (zctype>array-p type) (null (caddr type)))
    (zcerror "Attempt to make an array of indefinite arrays"))
  (list ':array type length))

(defsubst zctype>function-returning (type &optional param-types)
  "Makes the type of a function that returns the given type.  PARAM-TYPES, if supplied,
   is NCONS of a list of the form ((name type arghack) ...)."
  (list* ':function type param-types))

(defun zctype>struct-of (class tag-or-elts)
  "Makes a structure of the specified class (which must be :STRUCT, :PACKED-STRUCT,
   or :UNION) and with the specified tag or element alist."
  (when (not (memq class '(:struct :packed-struct :union)))
    (zcerror "Internal error: Bad structure class ~A" class))
  (cons class tag-or-elts))

(defun zctype>enum-of (tag-or-elts)
  "Makes the type of an enumeration with the specified tag or (NAME . VALUE)
   alist."
  `(:enum . ,tag-or-elts))

(defun zctype>bits-of (type size)
  "Makes a bit field descriptor.  All we know now is the size."
  `(:bits ,type ,size nil))


; ================================================================
; Miscellaneous type manipulation.

(defun zctype>widen-integral (type)
  "If TYPE is a short integral type, widens it to INT or UNSIGNED as appropriate.
   Returns other types unchanged."
  (if (zctype>shorter-than-int-p type)
      (if (zctype>unsigned-p type) (zctype>unsigned) (zctype>int))
    type))

(defun zctype>canonicalize-pointer (type)
  "Coerces an /"array type/" to a pointer type.  Arrays are equivalent
   to pointers except that they get special storage-management treatment.
   Non-array//pointer types are left alone; passing one is not an error."
  (if (and (listp type) (eq (car type) ':array))
      (cons ':pointer (cdr type))
    type))

(defun zctype>pointer-deref-type (type)
  "What type does this pointer point to?"
  (cond ((zctype>pointer-p type) (cadr type))
	((nlet ((hook (zctype>get-lispval-hook type)))
	   (and hook (funcall hook :deref-type type))))
	(T (zcerror "Internal error: ~A is not a pointer type" type))))

(defun zctype>array-length (type)
  "How many elements does this array type have?"
  (if (zctype>array-p type)
      (caddr type)
    (zcerror "Internal error: ~A is not an array type" type)))

(defun zctype>set-array-length (type length)
  "Sets the length of this array type, which must have been of indefinite length."
  (if (and (zctype>array-p type) (or (null (caddr type)) (null length)))
      (setf (caddr type) length)
    (zcerror "Internal error: ~A is not the type of an array of indefinite length"
	     type)))

(defun zctype>pointerize-indefinite-array (type)
  "Modifies an array type of indefinite length into a pointer type."
  (if (and (zctype>array-p type) (null (caddr type)))
      (progn (setf (car type) ':pointer) (setf (cddr type) nil))
    (zcerror "Internal error: ~A is not the type of an array of indefinite length"
	     type)))

(defun zctype>function-return-type (type)
  "What type does this function return?"
  (if (zctype>function-p type)
      (cadr type)
    (zcerror "Internal error: ~A is not a function type" type)))

(defun zctype>add-function-param-types (func-type param-types)
  "Modifies FUNC-TYPE to specify that its parameter types are PARAM-TYPES."
  (zctype>function-return-type func-type)         ; error checking only.
  (rplacd (cdr func-type) (ncons param-types)))

(defun zctype>function-param-types-p (type)
  "Does this function type know its parameter types?"
  (zctype>function-return-type type)         ; error checking only.
  (cddr type))

(defun zctype>function-param-types (type)
  "What are the types of the parameters of this function?"
  (zctype>function-return-type type)         ; error checking only.
  (caddr type))

(defun zctype>lispval-type (type)
  "Given a specialized lispval type, returns the specialization info."
  (if (not (and (listp type) (eq (car type) ':lispval)))
      (zcerror "Internal error: ~A is not a lispval type" type)
    (cdr type)))

(defun zctype>get-lispval-hook (type)
  "Gets the handler associated with the /"hooked/" lispval type TYPE."
  (and (zctype>lispval-p type)
       (listp type)
       (nlet ((lispval-type (zctype>lispval-type type))
	      ((name (if (nlistp lispval-type) lispval-type (car lispval-type)))))
	 (get name 'lispval-hook))))

(defun zctype>set-lispval-hook (name handler)
  "Associates a handler with the hook NAME."
  (setf (get name 'lispval-hook) handler))

(defun zctype>lispval-hook (name &rest args)
  "Create a /"hooked/" lispval type.  Error if the hook has not been initialized with
   ZCTYPE>SET-LISPVAL-HOOK."
  (or (get name 'lispval-hook)
      (zcerror "No hook for lispval type ~S" name))
  (zctype>lispval (if (null args) name (cons name (copylist args)))))

(defun zctype>struct-class (type)
  "Returns :STRUCT, :PACKED-STRUCT or :UNION appropriately."
  (if (zctype>struct-p type)
      (car type)
    (zcerror "Internal error: ~A is not a structure type" type)))

(defun zctype>struct-tag (struct-type)
  "Given a structure type, returns its tag, or NIL if the elements are explicitly
   specified."
  (zctype>struct-class struct-type)             ; for error checking only.
  (and (nlistp (cdr struct-type)) (cdr struct-type)))

(defun zctype>struct-elements (struct-type env)
  "Given a structure type, returns an element alist.  Gives an error if the
   structure type name is still undeclared."
  (zctype>struct-class struct-type)             ; for error checking only.
  (if (listp (cdr struct-type))
      (cdr struct-type)
    (or (cdr (zcenv>struct//union-type (cdr struct-type) env))
	(zcerror "Undeclared struct//union tag: ~A" (cdr struct-type)))))

;; These are for looking at the elements of the list returned by the above.
(defsubst zctype>eltpr-name (eltpr) (car eltpr))
(defsubst zctype>eltpr-offset (eltpr) (cadr eltpr))
(defsubst zctype>eltpr-type (eltpr) (cddr eltpr))

(defun zctype>struct-elements-p (struct-type)
  "Does this struct-type have its elements explicitly specified?"
  (zctype>struct-class struct-type)             ; for error checking.
  (listp (cdr struct-type)))

(defun zctype>declare-struct//union-type (name type env)
  "Declares a structure or union type.  If the type is already declared, and the
   new declaration is different, this redefines the type in a way that will be
   visible to all users of it immediately, and issues a warning that things may
   need to be recompiled."
  (zctype>struct-class type)                    ; for error checking.
  (let ((oldtype (zcenv>struct//union-type name env t)))
    (when (and oldtype (not (zctype>match oldtype type env)))
      (zcwarn "~&Structure or union type ~A being redefined -- be sure to~%  ~
		   recompile everything that uses it!" name)
      (rplaca oldtype (car type))
      (rplacd oldtype (cdr type))))
  (let ((putprop (zcenv>declare-struct//union name type env)))
    (and (zcenv>global-env-p env)
	 `(progn 'compile ,putprop
		 (putprop ',name 'zclstn>aggregate-invoke
			  'named-structure-invoke)))))

(defun zctype>verify-struct (class tag elements env)
  "Does several error checks on a structure type just created."
  (let ((dup (alist-has-duplicates-p elements)))
    (when dup
      (zcerror "Component name ~A used more than once in struct//union definition"
	       dup)))
    (dolist (elt elements)
      (when (zctype>void-p (zctype>eltpr-type elt))
	(zcerror "VOID elements may not appear in structs or unions"))
      (when (zctype>bits-p (zctype>eltpr-type elt))
	(when (eq class :union)
	  (zcerror "Unions may not contain bit fields"))
	(when (not (zctype>integer-p (cadr (zctype>eltpr-type elt))))
	  (zcerror "A bit field may only hold a signed or unsigned integer, not a ~a"
		   (cadr (zctype>eltpr-type elt))))
	(when (> (caddr (zctype>eltpr-type elt))
		 #+3600 63 #+Lambda 24 #+CADR 24 #+Explorer 63)
	  (zcerror "Bit field width ~D (for ~A) too long"
		   (caddr (zctype>eltpr-type elt)) (zctype>eltpr-name elt)))))
    (when tag
      (zctype>verify-struct-tag (cons class elements) tag env)))

(defun zctype>verify-struct-tag (type tag env)
  (cond ((zctype>struct-p type)
	 (when (and (symbolp (cdr type)) (eq (cdr type) tag))
	   (zcerror "Attempt to define struct//union type ~A in terms of itself"
		    tag))
	 (mapc #'(lambda (pair)
		   (zctype>verify-struct-tag (zctype>eltpr-type pair) tag env))
	       (zctype>struct-elements type env)))
	((zctype>array-p type)
	 (zctype>verify-struct-tag (zctype>pointer-deref-type type) tag env))))

(defun zctype>union-first-element (type env)
  "If TYPE is not a union type, returns it; otherwise returns its first non-union
   element."
  (if (neq (zctype>struct-class type) ':union)
      type
    (zctype>union-first-element
      (zctype>eltpr-type (car (zctype>struct-elements type env))) env)))

(defun zctype>declare-enum-type (tag tag-type elt-alist elt-type env)
  "Declares an enumeration type.  If the type is already declared, and the new
   declaration is different, this redefines the type in a way that will be visible
   to all users of it immediately, and issues a warning that things may need to be
   recompiled."
  (when (and tag (zcenv>global-env-p env))
    (let ((oldtype (zcenv>enum-type tag env)))
      (when (and oldtype (not (equal oldtype tag-type)))
	(zcwarn "~&Enumeration type ~A being redefined -- be sure to recompile~%  ~
		     everything that uses it or the constants it defines!" tag))))
  (zcenv>declare-enum tag tag-type elt-alist elt-type env))

(defun zctype>constant-type (const)
  "Computes the type of a constant."
  (cond ((fixp const) (if (= const 0) (zctype>zero) (zctype>int)))
	#-3600
	((small-floatp const) (zctype>float))
	#-3600
	((flonump const) (zctype>double))
	#+3600
	((sys:single-float-p const) (zctype>float))
	#+3600
	((sys:double-float-p const) (zctype>double))
	((or (stringp const)
	     (and (arrayp const) (eq (array-type const) 'art-8b)))
	 (zctype>array-of (zctype>char) (array-active-length const)))
	(t (zcerror "Internal error: don't know the type of constant ~S" const))))

; The EXPECTED list this looks at is produced by ZCDECL>PARAM-DECLARATIONS.
(defun zctype>arglist-type-check (actual expected callee caller)
  "Checks the types of function arguments against the types the function expects.
   Issues a warning if any mismatch is found."
  (labels ((loop (actual expected iarg optp restp)
	     (nlet ((restp (or restp (eq (third (car expected)) ':restarg)))
		    ((optp (or optp restp (eq (third (car expected)) ':optarg)))))
	       (cond ((null expected)
		      (when (and actual (not restp))
			(zcwarn "Too many arguments (~D expected; ~D supplied) in call ~
			       to ~A~@[ from ~A~]"
				iarg (+ iarg (length actual)) callee caller)))
		     ((null actual)
		      (when (and expected (not optp))
			(zcwarn "Too few arguments (~D expected; ~D supplied) in call ~
			       to ~A~@[ from ~A~]"
				(+ iarg (length expected)) iarg callee caller)))
		     ((and (not (zctype>equal (car actual)
					      (second (car expected))))
			   (not (and restp
				     (zctype>void-p (second (car expected))))))
		      (zcwarn "Wrong type to argument ~D of ~D (/"~A/" expected, ~
			     /"~A/" supplied)~@[ in call from ~A~]"
			      (first (car expected)) callee
			      (zclstn>type-string (second (car expected)))
			      (zclstn>type-string (car actual))
			      caller)
		      (loop (cdr actual) (cdr expected) (1+ iarg) optp restp))
		     (t (loop (cdr actual) (cdr expected)
			      (1+ iarg) optp restp))))))
    (loop actual expected 0 nil nil)))

(defun zctype>sizeof-in-scale (type env &optional unpacked)
  "Given a type, returns the size of its representation, in whatever the canonical
   scale is for that type.  Second value is the scale: :8B, :16B, or :Q."
  (cond ((and unpacked (zctype>number-p type)) (values 1 :Q))
	((or (zctype>char-p type) (zctype>signed-char-p type))
	 (values 1 :8B))
	((or (zctype>signed-short-p type) (zctype>unsigned-short-p type))
	 (values 1 :16B))
	((zctype>number-p type) (values 1 :Q))
	((zctype>lispval-p type) (values 1 :Q))
	((zctype>enum-p type) (values 1 :Q))
	((zctype>canonicalized-pointer-p type) (values 2 :Q))
	((zctype>function-pointer-p type) (values 1 :Q))
	((and (zctype>array-p type) (null (caddr type)))
	 (values 0                           ; indefinite arrays have "zero size".
		 (zctype>type-scale (cadr type))))
	((zctype>array-p type)
	 (nlet ((elt-size scale (zctype>sizeof-in-scale (cadr type) env)))
	   (values (* (caddr type) elt-size) scale)))
	((not (zctype>struct-p type))
	 (zcerror "Internal error: type ~A not accounted for!" type))
	((memq (zctype>struct-class type) '(:struct :packed-struct))
	 (values (zctype>struct-size type env) :Q))
	((eq (zctype>struct-class type) :union)
	 (values (gmap (:max)
		       #'(lambda (elt)
			   (if (eq (zctype>eltpr-name elt) ':end) 0
			     (zctype>sizeof-in-qs (zctype>eltpr-type elt) env)))
		       (:list (zctype>struct-elements type env)))
		 :Q))
	(t (zcerror "Internal error: type ~A not accounted for!" type))))

(defun zctype>struct-size (type env)
  "The size of a struct or union type, in Qs."
  (zctype>struct-elt-offset ':end type env))

(defun zctype>sizeof-in-qs (type env)
  "Given a type, returns the size of its representation, in Q-scale."
  (nlet ((size scale (zctype>sizeof-in-scale type env)))
    (ceiling size (// 4 (zctype>scale-size scale)))))

(defun zctype>scale-size (scale)
  "Given a scale (:8B, :16B, or :Q) returns its size in :8B scale."
  (or (cdr (assq scale '((:8B . 1) (:16B . 2) (:Q . 4))))
      (zcerror "Internal error: scale ~S encountered" scale)))

(defun zctype>rescale-index (index old-scale new-scale)
  "Converts a structure index from one scale to another.  Note the possibility of
   padding being added."
  (// (upto-next (* index (zctype>scale-size old-scale))
		 (zctype>scale-size new-scale))
      (zctype>scale-size new-scale)))

(defun zctype>type-scale (type &optional unpacked)
  "Given a type, returns the scale (:8B, :16B, or :Q) appropriate to it.  UNPACKED
   means the object occurs at top level or in an unpacked struct."
  (cond ((and unpacked (zctype>number-p type)) :Q)
	((or (zctype>char-p type) (zctype>signed-char-p type)) :8B)
	((or (zctype>signed-short-p type) (zctype>unsigned-short-p type)) :16B)
	((zctype>number-p type) :Q)
	((zctype>lispval-p type) :Q)
	((zctype>void-p type) :Q)
	((zctype>bits-p type) :Q)
	((zctype>enum-p type) :Q)
	((zctype>array-p type) (zctype>type-scale (cadr type) nil))
	((zctype>pointer-p type) :Q)
	((zctype>struct-p type) :Q)
	((zctype>function-p type) :Q)        ; Used by ZCTYPE>MATCH.
	(t (zcerror "Internal error: type ~A not accounted for!" type))))

(defun zctype>scale-type (scale)
  "Given a scale, returns the /"default/" type for which it is appropriate.  This
   is used to guess the equivalent C type of Lisp objects such as strings."
  (selectq scale
    (:Q :int)
    (:16B :unsigned-short)
    (:8B :char)))

(defun zctype>sizeof (type env)
  "Given a type, returns the size of its Zeta-C representation, in bytes."
  (nlet ((size scale (zctype>sizeof-in-scale type env)))
    (* size (zctype>scale-size scale))))

(defun zctype>element-count (&rest ignore)
  "This function's old semantics no longer make sense."
  (zcerror "Internal error: ZCTYPE>ELEMENT-COUNT called"))

(defun zctype>struct-elt-offset (elt type env)
  "Given an element name ELT and a struct type TYPE, computes the offset in the
   structure at which ELT is to be found, or NIL if ELT is not found.  This
   offset is in the scale appropriate to the element."
  (cadr (assq elt (zctype>struct-elements type env))))

(defun zctype>struct-offset-elt (offset scale type env &optional top-level)
  "Inverse of ZCTYPE>STRUCT-ELT-OFFSET.  Returns three values: the name of the
   element, its offset (which will be  OFFSET) and its type.  Offsets are in
   SCALE.  If TOP-LEVEL is non-NIL and the last element of the struct is an array,
   it is treated as of effectively infinite length; otherwise, NIL is returned if
   the OFFSET is past the end.  T is returned as the fourth value in this case."
  (do ((elts (zctype>struct-elements type env) (cdr elts))
       (8b-offset (* offset (zctype>scale-size scale))))
      ((eq (caar elts) ':end))
    (nlet ((elt-offset (zctype>eltpr-offset (car elts)))
	   (elt-type (zctype>eltpr-type (car elts)))
	   ((elt-scale (zctype>type-scale elt-type)))
	   (next-offset (zctype>eltpr-offset (cadr elts)))
	   (next-scale (zctype>type-scale (or (zctype>eltpr-type (cadr elts))
					      (zctype>int)))))
      (when (or (< 8b-offset (* next-offset (zctype>scale-size next-scale)))
		(and top-level
		     (eq (zctype>eltpr-name (cadr elts)) ':end)
		     (zctype>array-p elt-type)))
	(return
	  (values (zctype>eltpr-name (car elts))
		  (zctype>rescale-index elt-offset elt-scale scale)
		  elt-type
		  ( 8b-offset (* next-offset
				  (zctype>scale-size next-scale)))))))))


; ================================================================
; Variable declaration processing.

(defun zcdecl>external-declaration (decl)
  "Processes an external declaration, returning three values; 1: a result-list;
   2: a list of top-level putprop forms to accomplish the type declarations; and
   3: the storage class.  If the storage-class is NIL or ':STATIC, the result-list
   is a list of (variable initializer) pairs; if ':EXTERN, it's a list of variables
   to be declared special; otherwise, it should be ignored."
  (zcdecl>process-one-decl decl (zcenv>global-env) ':external))

(defun zcdecl>local-declarations (decls env)
  "Processes a list of local declarations, adding them to the environment ENV,
   and returns four values: a list of initialization clauses for locals; a list
   of any variables to be declared special; a list of initialization clauses for
   statics; and a list of toplevel forms (used for externs)."
  (nlet ((auto-inits specials static-inits toplevel-forms
	  (zcdecl>local-declarations-1 decls env nil nil nil nil)))
    (map #'(lambda (tail)
	     (when (assq (caar tail) (cdr tail))
	       (zcerror "Static ~A declared more than once in the same block"
			(caar tail)))
	     (when (memq (caar tail) specials)
	       (zcerror "Variable ~A declared static and extern in the same block"
			(caar tail))))
	 static-inits)
    (map #'(lambda (autos)
	     (when (memq (caar autos) specials)
	       (zcerror "Variable ~A declared both auto and extern in one block"
			(caar autos)))
	     (when (memq (caar autos) (gmap:nlet>bound-vars static-inits))
	       (zcerror "Variable ~A declared both auto and static in one block"
			(caar autos)))
	     (when (assq (caar autos) (cdr autos))
	       (zcerror "Variable ~A declared more than once in one block"
			(caar autos))))
	 auto-inits)
    (values auto-inits specials static-inits toplevel-forms)))
(defun zcdecl>local-declarations-1 (decls env auto-inits specials static-inits
				    toplevel-forms)
  (if (null decls)
      (values auto-inits specials static-inits toplevel-forms)
    (multiple-value-bind (result-list toplev sclass)
	(zcdecl>process-one-decl (car decls) env ':local)
      (selectq sclass
	((nil :register :auto)
	 (zcdecl>local-declarations-1 (cdr decls) env
				      (nconc auto-inits result-list)
				      specials static-inits toplevel-forms))
	(:extern
	 (zcdecl>local-declarations-1 (cdr decls) env auto-inits
				      (nconc specials result-list) static-inits
				      (nconc toplevel-forms toplev)))
	(:static
	 (zcdecl>local-declarations-1 (cdr decls) env auto-inits specials
				      (nconc static-inits result-list)
				      toplevel-forms))))))

(defun zcdecl>param-declarations (params decls env)
  "Processes a list of parameter declarations.  Any undeclared parameter is assumed
   to be an int.  A declaration without a parameter is an error, as is a parameter
   that appears more than once in the parameter list.  Returns an alist of the form
   ((name type arghack) ...)."
  (let ((declared-params
	  (mapcan #'(lambda (decl)
		      (zcdecl>process-one-decl decl env ':parameter))
		  decls)))
    ;; Undeclared params are ints.
    (dolist (param params)
      (unless (memq param declared-params)
	(zcenv>declare-type param (zctype>int) env)))
    ;; Check for some error conditions.
    (dolist (declared-param declared-params)
      (unless (memq declared-param params)
	(zcerror "Variable ~A appeared in the parameter declarations, but is not one~@
		  of the parameters ~A" declared-param params))
      (when (and (eq (zcenv>get declared-param 'arghack env) ':restarg)
		 (neq declared-param (car (last params))))
	(zcerror "Variable ~A is declared a restarg, but is not the last parameter~@
		  in the parameter list ~A" declared-param params)))
    (let ((dup (list-has-duplicates-p declared-params)))
      (when dup (zcerror "Parameter ~A declared more than once" dup)))
    (let ((dup (list-has-duplicates-p params)))
      (when dup (zcerror "Parameter ~A appeared more than once in parameter list ~A"
			 dup params)))
    ;; And finally, the (name type arghack) list.
    (mapcar #'(lambda (param)
		(list param (zctype>widen-integral (zcenv>type param env))
		      (zcenv>get param 'arghack env)))
	    params)))

(defun zcdecl>function-declaration (decl param-decls env)
  "Processes a function declaration along with its parameter declarations,
   returning four values: the function's name, its type, its parameters, and a
   list of putprop forms to declare it (suitable for fasdumping)."
  (nlet ((alist declarers
	  (zcdecl>process-one-decl decl (zcenv>global-env) ':function))
	 ((func (caaar alist))
	  (type (cdar alist))
	  (params (cdaar alist))))
    (let ((param-types (zcdecl>param-declarations params param-decls env)))
      (zctype>add-function-param-types type param-types)
      (values func type params declarers))))

(defun zcdecl>struct-elt-declarations (decls env)
  "Given a list of structure element declarations, returns an alist of element
   names and types."
  (gmap (:values (:nconc) (:nconc))
	#'(lambda (decl)
	    (zcdecl>process-one-decl decl env ':struct-element))
	(:list decls)))

; Some kind of not-quite-syntactic error in a declaration.
(defflavor declaration-error (message) (error)
  (:initable-instance-variables))

(defun zcdecl>parser-declaration (decl env)
  "Processes a parameter or local declaration for the parser.  This is in case the
   decl is a typedef, which the parser will need to know about; but while we're at
   it, we go ahead and check for certain kinds of well-formedness errors."
  (zcdecl>process-one-decl decl env ':parser))

(defun zcdecl>process-one-decl (decl env context)
  "Given a declaration, returns three values; 1: an alist appropriate to the
   context, if any (see NOTE.TEXT); 2: a list of top-level forms suitable for
   fasdumping; and 3: the storage class, if it needs to be remembered."
  (nlet ((type context sclass top-level-type-definitions
	       (zcdecl>decl-typelist (car decl) context env))
	 ((result-list top-level-forms
	   (zcdecl>process-one-decl-1 type (cdr decl) context sclass env))))
    (values result-list (nconc top-level-type-definitions top-level-forms)
	    sclass)))
(defun zcdecl>process-one-decl-1 (type specs context sclass env)
  (gmap (:values (:nconc :id) (:list :id))
	#'(lambda (spec+init)
	    (nlet ((spec init (zcdecl>split-spec spec+init))
		   ((var var-type (zcdecl>invert-spec type spec context))))
	      (zcdecl>declare-one-name var var-type init context sclass env)))
	(:list specs)))

(defun zcdecl>declare-one-name (var type init context sclass env)
  "Does the appropriate thing to declare the variable in this context.  Returns two
   values; 1: a list of initializer forms, variable names, or whatever, as
   appropriate for the context (see NOTE.TEXT); and 2: a top-level putprop form, if
   the name is being declared globally, else nil."
  (selectq context
    (:external
     ;; zcenv>declare-type has to be done before zcdecl>process-static-initializer.
     (nlet ((altvar (and (eq sclass ':static)
			 (zcdecl>file-static-alternate-name var)))
	    ((type-forms
	       `(progn
		  ,(zcenv>declare-type (or altvar var) type env t)
		  ,(and altvar (zcenv>declare-static var altvar env))
		  ,(and (not (zctype>function-p type))
			(zcenv>declare-definition (or altvar var) type env))))))
       (values (zcdecl>process-static-initializer (or altvar var) type init env)
	       type-forms)))
    (:local
     (when (zcenv>global-env-p env)
       (zcerror "Internal error: declaring a local, ~A, in the global environment"
		var))
     (if (and (zctype>array-p type) (null (zctype>array-length type)))
	 (zctype>pointerize-indefinite-array type))
     (values (if (eq sclass ':static)
		 (let ((altvar (zcdecl>function-static-alternate-name var)))
		   (zcenv>declare-type altvar type env)
		   (zcenv>declare-static var altvar env)
		   (zcdecl>process-static-initializer altvar type init env))
	       ;; must declare type before processing initializer.
	       (zcenv>declare-type var type env)
	       (zcdecl>process-auto-initializer var type init env))
	     nil #| (No top-level putprop) |# ))
    (:extern
     (values `(,var . ,(zcdecl>auxiliary-vars var type))
	     `(progn ,(zcenv>declare-type var type (zcenv>global-env))
		     (zcprim>extern '(,var . ,(zcdecl>auxiliary-vars var type))))))
    (:function
     ;; VAR is actually a list of the form (name params).
     (when (eq sclass ':static)
       (let ((altname (zcdecl>file-static-alternate-name (car var))))
	 (zcenv>declare-static (car var) altname env)
	 (setf (car var) altname)))
     (zcenv>declare-definition (car var) type env)
     (values (ncons (cons var type))
	     `(progn ,(zcenv>declare-type (car var) type env)
		     ,(zcenv>declare-definition (car var) type env))))
    (:parameter
     (when (zctype>array-p type)
       (when (zctype>array-length type)
	 (zcwarn "Parameter ~A is declared as an array of length ~D; the length~%~
		    is being ignored, since arrays cannot be passed as arguments"
		 var (zctype>array-length type))
	 (zctype>set-array-length type nil))
       (zctype>pointerize-indefinite-array type))
     (let ((type (if (zctype>function-p type) (zctype>pointer-to type) type)))
       (zcenv>declare-type var type env))
     (if (memq sclass '(:optarg :restarg))
	 (zcenv>declare var sclass 'arghack env))
     (ncons var))
    (:struct-element
     (ncons (cons var type)))
    (:typedef
     (values nil (zcenv>typedef var type env)))
    ;; Only typedefs are recorded for the parser; and that happens above.
    (:parser)
    (otherwise
     (zcerror "Internal error: Unknown context ~A" context))))

(defun zcdecl>file-static-alternate-name (name)
  "Creates a name of the form <file>$<name>; unless used interactively (<file> is
   NIL), in which case it just returns <name>."
  (if (null (zcenv>source-loc-file)) name
    (let ((file (zcenv>source-loc-file)))
      (intern (format nil "~A$~A" (nsubstring file 0 (string-search-char #\. file))
		      name)))))

(defun zcdecl>function-static-alternate-name (name)
  "Finds a name of the form <func>$<name>$<i> that hasn't been used yet in this
   function."
  (declare (special zcprim>*defun-specials* zcprim>*expanding-defunc+*))
  (unless zcprim>*expanding-defunc+*
    (zcerror "A STATIC declaration in a block that's not inside a function??"))
  (labels ((try (i)
	     (let ((actname (intern (format nil "~A$~A$~D"
					    zcprim>*expanding-defunc+* name i))))
	       (if (memq actname zcprim>*defun-specials*)
		   (try (1+ i))
		 (push actname zcprim>*defun-specials*)
		 actname))))
    (try 1)))

(defun zcdecl>auxiliary-vars (var type)
  "Returns a list of auxiliary variables used by VAR."
  (and (not (zctype>function-p type))
       (cons (zcprim>variable-address-var var)
	     (and (zctype>canonicalized-pointer-p type)
		  (nlet ((array-sym index-sym (zcprim>pointer-var-pair var)))
		    (list array-sym index-sym))))))

(defconst zcdecl>*storage-class-specifiers*
	  '((c:|extern| . :extern) (c:|static| . :static)
	    (c:|register| . :register) (c:|auto| . :auto)
	    (c:|restarg| . :restarg) (c:|optarg| . :optarg)
	    (c:|typedef| . :typedef)))

(defconst zcdecl>*primitive-types*
	  '((c:|char| . :char) (c:|int| . :int)
	    (c:|float| . :float) (c:|double| . :double)
	    (c:|lispval| . :lispval) (c:|void| . :void)
	    (nil . :void))
  "The primitive type /"nouns/", and their corresponding internal types.")

(defconst zcdecl>*type-adjectives*
	  '((c:|short| . (:short nil))
	    (c:|long| . (:long nil))
	    (c:|signed| . (nil :signed))
	    (c:|unsigned| . (nil :unsigned)))
  "The primitive type adjectives.")

(defun zcdecl>decl-typelist (typelist context env)
  "Returns values: 1: type; 2: context; 3: storage class to be remembered, if any;
   and 4: top-level putprop forms needed to establish the type."
  (nlet ((sclass (cdr (assq (car typelist) zcdecl>*storage-class-specifiers*)))
	 ((typelist (if sclass (cdr typelist) typelist))
	  (sclass context (zcdecl>check-storage-class sclass context))
	  ((type top-level-forms
	    (zcdecl>decl-typelist-1 typelist context env nil nil)))))
    (values type context sclass top-level-forms)))
(defun zcdecl>decl-typelist-1 (typelist context env length sign)
  (cond ((assq (car typelist) zcdecl>*storage-class-specifiers*)
	 ;; Actually, the grammar prohibits this anyway, but it might not always
	 ;; because of ZETA-C extensions.
	 (zcerror "Storage class ~A appears in the middle of a type specifier;~@
		   only one storageclass is allowed, and it must be first."
		  (car typelist)))
	((listp (car typelist))
	 (if (eq (caar typelist) 'c:|enum|)
	     (zcdecl>canonicalize-enum-type (cdar typelist) env)
	   (let ((kind (cdr (assq (caar typelist)
				  '((c:|struct| . :struct)
				    (c:|packed_struct| . :packed-struct)
				    (c:|union| . :union))))))
	     (if (not kind)
		 (zcerror "Internal error: Bad complex type specifier ~A"
			  (car typelist))
	       (zcdecl>canonicalize-struct-type kind (cdar typelist) env)))))
	((assq (car typelist) zcdecl>*type-adjectives*)
	 (nlet ((length sign (zcdecl>check-adjective (car typelist) length sign)))
	   (zcdecl>type-modified (zcdecl>decl-typelist-1 (cdr typelist) context env
							 length sign)
				 (car typelist))))
	(t (let ((type (if typelist
			   (or (cdr (assq (car typelist) zcdecl>*primitive-types*))
			       (zcenv>typedef-type (car typelist) env))
			 (zctype>int))))
	     (when (not type)
	       (zcerror "Internal error: undefined type name ~A" (car typelist)))
	     type))))

(defun zcdecl>check-adjective (adj length sign)
  "Checks that the adjective ADJ does not conflict with the existing LENGTH and
   SIGN; returns the new length and sign as two values."
  (nlet ((meaning (cdr (assq adj zcdecl>*type-adjectives*)))
	 ((new-length (car meaning))
	  (new-sign (cadr meaning))))
    (when (null meaning) (zcerror "Internal error: unknown adjective ~A (??)" adj))
    (when (and length new-length (neq length new-length))
      (zcerror "Type words ~A and ~A conflict" length new-length))
    (when (and sign new-sign (neq sign new-sign))
      (zcerror "Type words ~A and ~A conflict" sign new-sign))
    (values (or length new-length) (or sign new-sign))))

(defconst zcdecl>*type-modifications*
	  '((:char . (:error :error :signed-char :char))
	    (:signed-char . (:error :error :signed-char :error))
	    (:short . (:short :error :short :unsigned-short))
	    (:int . (:short :long :int :unsigned))
	    (:long . (:error :long :long :unsigned-long))
	    (:unsigned . (:unsigned-short :unsigned-long :error :unsigned))
	    (:unsigned-short . (:unsigned-short :error :error :unsigned-short))
	    (:unsigned-long . (:error :unsigned-long :error :unsigned-long))
	    (:float . (:error :double :error :error))
	    (:double . (:error :double :error :error))
	    (:lispval . (:error :error :error :error))
	    (:void . (:error :error :error :error)))
  "The type modification chart.  The cdrs are of the form (<short> <long> <signed>
/<unsigned>), showing what happens to each type when it is thus modified.")

(defun zcdecl>type-modified (type adj)
  "Processes a type possibly modified by adjective ADJ, returning the new type."
  (let ((modlist (cdr (assq type zcdecl>*type-modifications*))))
    (when (null modlist)
      (zcerror "Internal error: type ~A has no modlist" type))
    (nlet ((place (nth? adj 'c:(|short| |long| |signed| |unsigned|)))
	   ((mod-type (nth place modlist))))
      (when (eq mod-type ':error)
	(zcerror "Illegal type adjective combination: ~A applied to ~A" adj type))
      mod-type)))

(defconst zcdecl>*storage-class-contexts*
	  '((:register . (:parameter :local))
	    (:static . (:external :local :function))
	    (:auto . (:parameter :local))
	    (:extern . (:external :local))
	    (:typedef . (:external :parameter :local))
	    (:restarg . (:parameter))
	    (:optarg . (:parameter))))

(defun zcdecl>check-storage-class (sclass context)
  "Given a storage class (possibly NIL) and a context, verifies that the storage
   class is valid in the context, and returns 1: the storage class if it needs to
   be remembered and 2: the context, possibly modified by the storage class."
  (if (and sclass
	   (not (memq context (cdr (assq sclass zcdecl>*storage-class-contexts*))))
	   (neq context ':parser))
      (zcerror "Illegal use of storage class ~A in context ~A" sclass context))
  (let ((context (if (memq sclass '(:extern :typedef)) sclass context)))
    (if (memq sclass '(:register :extern :static :restarg :optarg))
	(values sclass context)
      (values nil context))))

(defun zcdecl>split-spec (spec)
  "Splits a spec into an initializer and the typespec proper."
  (if (and (listp spec) (eq (car spec) 'c:=))
      (values (cadr spec) (caddr spec))
    (values spec nil)))

(defun zcdecl>invert-spec (type spec context)
  "Given a type and a var-spec, returns two values: 1: the variable being declared
   and 2: its inverted type."
  (cond ((nlistp spec) (values spec type))
	((eq (car spec) 'c:*)
	 (zcdecl>invert-spec (zctype>pointer-to type) (cadr spec) context))
	((eq (car spec) 'c:fcn+)
	 (zcdecl>invert-function-spec type spec context))
	((eq (car spec) 'c:[])
	 (zcdecl>invert-spec
	   (zctype>array-of type (and (caddr spec)
				      (zcprim>eval-constant-int (caddr spec))))
	   (cadr spec) context))
	((eq (car spec) 'c:bits+)
	 (when (not (eq context ':struct-element))
	   (zcerror "Bit fields only permitted in struct element declarations"))
	 (when (not (symbolp (cadr spec)))
	   (zcerror "Illegal bit field specification"))
	 (zcdecl>invert-spec
	   (zctype>bits-of type (zcprim>eval-constant-int (caddr spec)))
	   (cadr spec) context))
	(t (zcerror "Internal error: can't invert type spec ~A" spec))))

;; Will get more complex to handle function prototypes.
(defun zcdecl>invert-function-spec (type spec context)
  (if (and (nlistp (cadr spec)) (eq context ':function))
      ;; special case for defunc+.
      (values (cdr spec) (zctype>function-returning type))
    (zcdecl>invert-spec (zctype>function-returning type) (cadr spec)
			context)))

(defun zcdecl>invert-abstract-decl (decl env)
  "Inverts an abstract declaration, that is, one used in casts and sizeofs."
  (multiple-value-bind (ignore type)
      (zcdecl>invert-spec (zcdecl>decl-typelist (car decl) 'abstract env)
			  (cadr decl) 'abstract)
    type))

(defun zcdecl>canonicalize-struct-type (kind struct-spec env)
  "Given a kind (struct or union) and a struct-spec, returns the type of the
   structure.  A struct-spec is a list of the form (name . decls), where either the
   name or the decls can be NIL.  An undefined structure name is just used as is,
   since forward references are sometimes legal.  Returns two values: the
   canonicalized type, and a list of top-level forms to define any new structure
   names declared."
  (if (cdr struct-spec)
      (nlet ((elts nested-struct-defn-forms
	      (zcdecl>struct-elt-declarations (cdr struct-spec) env))
	     ((elts (zcdecl>compile-struct-elts kind elts env))))
	(zctype>verify-struct kind (car struct-spec) elts env)
	(nlet ((type (zctype>struct-of kind (or (car struct-spec) elts)))
	       ((form (and (car struct-spec)
			   (zctype>declare-struct//union-type
			     (car struct-spec) (zctype>struct-of kind elts)
			     env)))))
	  (values type (cons-if-non-nil form nested-struct-defn-forms))))
    (if (car struct-spec)
	(let ((type (zcenv>struct//union-type (car struct-spec) env t)))
	  (when (and type
		     (not (or (eq kind (zctype>struct-class type))
			      (and (eq kind :struct)
				   (eq (zctype>struct-class type)
				       ':packed-struct)))))
	    (zcerror "Tag ~A was defined as a ~A, but is being used as a ~A"
		     (car struct-spec) (zctype>struct-class type) kind))
	  (zctype>struct-of (if type (zctype>struct-class type) kind)
			    (car struct-spec)))
      (zcerror "Internal error: structure tag and elements are both null"))))

(defun zcdecl>compile-struct-elts (class elts env)
  "Processes a struct (elt . type) alist, compiling in the offsets.  Returns a
   compiled struct alist, of the form (elt offset . type) where each offset is in
   the appropriate scale."
  (do ((elts elts (cdr elts))
       (offset 0)
       (bit-offset 0)
       (unpacked (eq class ':struct))
       (compiled-alist nil))
      ((null elts)
       (nreverse (cons `(:end ,(+ (ceiling offset 4) (if (plusp bit-offset) 1 0)))
		       compiled-alist)))
    (let ((name (caar elts))
	  (type (cdar elts)))
      (if (zctype>bits-p type)
	  (if (zerop (caddr type))
	      (progn (when name
		       (zcerror "0-width bit field (~A) must be unnamed" name))
		     (setq offset (upto-next (+ offset (ceiling bit-offset 8)) 4))
		     (setq bit-offset 0))
	    (when (not (zerop (mod offset 4)))
	      (if unpacked (setq offset (upto-next offset 4))
		(setq bit-offset (* offset 8))
		(decf offset (mod offset 4))))
	    (when (> (+ bit-offset (caddr type)) 32.)
	      (setq bit-offset 0) (incf offset 4))
	    (when name                       ; leave gaps out of result.
	      (push `(,name ,(// offset 4)
		      . (:bits ,(cadr type) ,(caddr type) ,bit-offset))
		    compiled-alist))
	    (incf bit-offset (caddr type)))
	(when (not (zerop bit-offset))
	  (incf offset (ceiling bit-offset 8)) (setq bit-offset 0))
	(nlet ((elt-size elt-scale (zctype>sizeof-in-scale type env unpacked))
	       ((scale-size (zctype>scale-size elt-scale))))
	  (setq offset (upto-next offset scale-size))           ; alignment.
	  (push `(,name ,(ceiling offset scale-size) . ,type) compiled-alist)
	  (incf offset (* elt-size scale-size)))))))

(defun zcdecl>canonicalize-enum-type (enum-spec env)
  "Given an enum specification -- a cons of an optional tag and an optional list of
   names or (= <name> <exp>) forms -- returns the corresponding enumeration type.
   Performs all declarations implicit in the ENUM-SPEC, and returns as a second
   value a list of top-level forms to perform those declarations again."
  (let ((tag (car enum-spec))
	(names (cdr enum-spec)))
    (if names
	(nlet ((elt-alist (zcdecl>enum-elt-alist names))
	       ;; If global, we refer to it by tag so interactive updating works.
	       ((type (zctype>enum-of (or (and (zcenv>global-env-p env) tag)
					  elt-alist)))))
	  (when (alist-has-duplicates-p elt-alist)
	    (zcerror "Enumeration constant name ~A appears more than once in the~@
		      same ENUM declaration." (alist-has-duplicates-p elt-alist)))
	  ;; But we always declare the tag using the alist.
	  (zctype>declare-enum-type tag (zctype>enum-of elt-alist) elt-alist
				    type env)
	  (values type (and (zcenv>global-env-p env)
			    `((zcdecl>canonicalize-enum-type ',enum-spec ',env)))))
      (if tag
	  (let ((type (zcenv>enum-type tag env)))
	    (if type
		;; Again, if global, use tag.
		(if (zcenv>global-env-p env) (zctype>enum-of tag) type)
	      (zcerror "Enumeration tag ~A has not been declared" tag)))
	(zcerror "Internal error: enum tag and elements are both null")))))

(defun zcdecl>enum-elt-alist (names)
  "Given a list of names or (= <name> <exp>) forms, returns an alist associating
   names with integer values."
  (zcdecl>enum-elt-alist-1 names 0))
(defun zcdecl>enum-elt-alist-1 (names current-val)
  (and names
       (nlet ((name val (if (listp (car names))
			    (values (cadar names)
				    (zcprim>eval-constant-int (caddar names)))
			  (values (car names) current-val))))
	 (cons (cons name val) (zcdecl>enum-elt-alist-1 (cdr names) (1+ val))))))


; ================
; Initializer processing.  This requires a double tree-walk over the initializer
; and the thing being initialized, so we start with a small "init-ptr" abstraction
; that keeps a stack of pointers into the initializer.

; This is totally confusing and should get rewritten someday.

(defsubst zcdecl>init-list-p (init)
  (and (listp init) (eq (car init) 'c:list+)))

(defun zcdecl>init-ptr>create (init)
  (cons nil (and init (list 'c:list+ init))))

(defsubst zcdecl>init-ptr>where (init-ptr)
  "Returns the cons from which the next value will be taken."
  (if (car init-ptr) (cadar init-ptr) (cddr init-ptr)))

(defun zcdecl>init-ptr>push (init-ptr)
  (let ((where (zcdecl>init-ptr>where init-ptr)))
    (if (zcdecl>init-list-p (car where)) 
	(rplaca init-ptr (cons (last (car where))       ; for flat rep to rplacd
			       (cons (cdar where) (car init-ptr))))
	(rplaca init-ptr (cons nil (cons where (car init-ptr)))))))

(defun zcdecl>init-ptr>pop (init-ptr)
  (let ((prevframe (cddar init-ptr)))
    (if prevframe
	(setf (cadr prevframe)
	      (if (caar init-ptr)
		  (cdr (cadr prevframe))
		(cadar init-ptr))))
    (rplaca init-ptr (cddr (car init-ptr)))))

(defsubst zcdecl>init-ptr>read (init-ptr)
  (car (zcdecl>init-ptr>where init-ptr)))

(defun zcdecl>init-ptr>read++ (init-ptr)
  (let ((val (zcdecl>init-ptr>read init-ptr)))
    (if (car init-ptr) (pop (cadar init-ptr)))
    val))

(defun zcdecl>init-ptr>write++ (init-ptr frob)
  (rplaca (zcdecl>init-ptr>where init-ptr) frob)
  (if (car init-ptr) (pop (cadar init-ptr)))
  frob)

(defun zcdecl>init-ptr>nested-p (init-ptr)
  (caar init-ptr))

(defun zcdecl>init-ptr>eolp (init-ptr)
  (nlistp (zcdecl>init-ptr>where init-ptr)))


(defun zcdecl>process-auto-initializer (name type init env)
  "Takes a type and a user initializer (or NIL), and produces either NIL or a list
   of the form: ((<name> <type> <init-exp> [<freelist-var>])).  <init-exp> will be
   NIL if there was no user initializer."
  (if (zctype>function-p type) nil
    (if (zctype>scalar-p type)
	`((,name ,type
	   ,(and init (zcdecl>init-trans (if (zcdecl>init-list-p init)
					     (cadr init) init)
					 type env))))
      (let ((init-ptr (zcdecl>init-ptr>create init))
	    (freelist-var (gensym)))
	(when init
	  (zcdecl>initializer-compile init-ptr type env))
	`((,name ,type
	   ,(zcdecl>auto-aggregate-initializer name (caddr init-ptr) type
					       env freelist-var)
	   ,freelist-var))))))

(defun zcdecl>auto-aggregate-initializer (name init type env freelist-var)
  "A form to initialize an automatic aggregate."
  `(let ((%%temp (or ,freelist-var
		     ;; All this wants from the env is structure types.  Maybe we
		     ;; should arrange to extract those?
		     (zcdecl>create-or-reuse-structure nil ',type ',env))))
     ;; We chain the freelist through an element of the array-leader.
     (setq ,freelist-var (zcprim>array-freelist-link %%temp))
     (setf (car (zcprim>array-desc %%temp)) ',name)
     ,@(and init `((zcdecl>fill-structure (zcdecl>init-ptr>create ',init) %%temp)))
     %%temp))

(defun zcdecl>process-static-initializer (name type init env)
  "Takes a type and a user initializer (or NIL) for an object of static extent, and
   produces a list of the form ((<name> [<init-exp>]) ...)."
  (if (zctype>function-p type) nil
    (let ((init-ptr (zcdecl>init-ptr>create init)))
      (zcdecl>initializer-compile init-ptr type env)   ; can modify (cdr init-ptr)
      `((,name (zcdecl>interpret-initializer ',name ',(caddr init-ptr) ',type ',env
					     ',*source-location*))
	. ,(mapcar #'ncons (zcdecl>auxiliary-vars name type))))))

(defun zcdecl>interpret-initializer (name init type env
				     &optional *source-location*)
  "Processes a static initializer at runtime, creating the initial value."
  (let ((old-val (zcdecl>create-or-reuse-structure
		   (and name (boundp name) (symeval name)) type env)))
    (zcenv>declare-type name type (zcenv>global-env))
    ;; The name has to be bound to the structure before calling fill-structure,
    ;; in case of a self-referential initializer.
    (set name old-val)
    (when (and (arrayp old-val) (null (car (zcprim>array-desc old-val))))
      (setf (car (zcprim>array-desc old-val)) name))
    (let ((old-val (zcdecl>fill-structure (zcdecl>init-ptr>create init) old-val)))
      (if (zctype>canonicalized-pointer-p type)
	  (zcdecl>setup-pointer-aux-vars name old-val type)
	(zcdecl>setup-scalar-aux-vars name type))
      old-val)))

(defun zcdecl>setup-pointer-aux-vars (name old-val type)
  (nlet ((array-sym index-sym (zcprim>pointer-var-pair name))
	 (address-sym (zcprim>variable-address-var name)))
    (set array-sym (car old-val))
    (set index-sym (cdr old-val))
    ;; Yeesh.  Is anybody ever gonna look at this cons?  Well, maybe from Lisp.
    (without-interrupts
      (nlet ((car-loc (car-location old-val))
	     ((cdr-loc (%make-pointer-offset dtp-locative car-loc 1))
	      ((car-cdr-code (%p-cdr-code car-loc))
	       (cdr-cdr-code (%p-cdr-code cdr-loc)))))
	(%p-store-tag-and-pointer car-loc dtp-one-q-forward
				  (value-cell-location array-sym))
	(%p-store-cdr-code car-loc car-cdr-code)
	(%p-store-tag-and-pointer cdr-loc dtp-one-q-forward
				  (value-cell-location index-sym))
	(%p-store-cdr-code cdr-loc cdr-cdr-code)))
    ;; Here we make the address array (used to take a pointer to this pointer).
    (set address-sym (zcdecl>pointer-address-var name type (zcenv>global-env)
						 array-sym index-sym))))

; Automatic test to see if the hack below will work.
(eval-when (compile)
  (unless (= 1 (%pointer-difference (function-cell-location 'foo)
				    (value-cell-location 'foo)))
    (ferror "Hack putting index in function cell won't work here
/(see ZCDECL>POINTER-ADDRESS-VAR)."))) 

; Called at runtime by forms generated by the above.
(defun zcdecl>pointer-address-var (name type env array-sym index-sym)
  ;; Hack: instead of having one-q-forwards in the aarray, we displace the aarray
  ;; to the value AND function cells of the array symbol (which happen to be
  ;; contiguous in that order).  On the 3600, we then one-q-forward the function
  ;; cell of the array symbol to the value cell of the index symbol, but this
  ;; doesn't seem to work on A-machines, so we do it the other way around.
  ;; (Reason: one-q-forwards in the aarray don't work in the LMITI system.)
  (nlet ((array-loc (value-cell-location array-sym))
	 (index-loc (value-cell-location index-sym))
	 ((aarray (make-array 2 :named-structure-symbol 'value-cell
			      :displaced-to array-loc
			      :leader-list (zcprim>array-leader-init
					     name (zctype>array-of type 1) env)))))
    (and (boundp index-sym) (fset array-sym (symeval index-sym)))
    #+3600 (%p-store-tag-and-pointer (function-cell-location array-sym)
				     dtp-one-q-forward index-loc)
    #-3600 (%p-store-tag-and-pointer index-loc dtp-one-q-forward
				     (function-cell-location array-sym))
    (zcprim>store-scale-slot :Q aarray aarray)
    aarray))

(defun zcdecl>setup-scalar-aux-vars (name type)
  ;; Everything except arrays and structures get address arrays.
  (unless (or (zctype>array-p type) (zctype>struct-p type))
    (let ((address-sym (zcprim>variable-address-var name)))
      (unless (and (boundp address-sym)
		   (arrayp (symeval address-sym))
		   (equal (arraydims (symeval address-sym)) '(art-q 1)))
	(set address-sym
	     (zcdecl>scalar-address-var name type (zcenv>global-env)))))))

;; Called at runtime by forms generated by the above.
(defun zcdecl>scalar-address-var (name type env)
  ;; TARRAY may have had something to do with strangeness of byte arrays on the
  ;; 3600.  Can't, at this juncture, remember what though.
  (nlet (; ?? WHY is this here??? (tarray (make-array 1))
	 (value-loc (value-cell-location name))
	 ((aarray (make-array 1 :type (zcprim>scale-art (zctype>type-scale type))
			      :displaced-to value-loc ; ?? was: tarray
			      :named-structure-symbol 'value-cell
			      :leader-list (zcprim>array-leader-init
					     name (zctype>array-of type 1) env)))))
    ;;(%p-store-tag-and-pointer (aloc tarray 0) dtp-one-q-forward value-loc)
    (zcprim>store-scale-slot (zctype>type-scale type) aarray aarray)
    aarray))

(defun zcdecl>create-or-reuse-structure (old-val type env &optional ignore)
  "Creates a structure suitable for TYPE, reusing pieces of OLD-VAL when possible."
  (let ((new-val (zcdecl>create-or-reuse-structure-1 old-val type env)))
    (when (and (neq old-val new-val) (arrayp old-val) (arrayp new-val))
      (structure-forward old-val new-val))
    new-val))
(defun zcdecl>create-or-reuse-structure-1 (old-val type env)
  (cond
    ((or (zctype>lispval-p type) (zctype>void-p type)) nil)
    ;; A number or pointer gets initialized to the user initializer or 0 / NULL.
    ((zctype>integer-p type) 0)
    ((zctype>double-p type) #+3600 0.0d0 #-3600 0.0)
    ((zctype>float-p type) #+3600 0.0 #-3600 0.0s0)
    ((zctype>function-pointer-p type)
     (if (and (symbolp old-val) (not (null old-val)))
	 old-val 'null-function-pointer))
    ((zctype>canonicalized-pointer-p type)
     (if (listp old-val)
	 (zcptr>cons nil 0 old-val)
       (zcptr>cons nil 0)))
    
    ;; An array gets an array of initializer values or the default, recursively.
    ((zctype>array-p type)
     (nlet ((len scale (zctype>sizeof-in-scale type env))
	    ((old-val (or (zcdecl>old-array-check old-val type env)
			  (make-array len
				      ':type (zcprim>scale-art scale)
				      ':initial-value 0
				      ':named-structure-symbol 'array
				      ':leader-list (zcprim>array-leader-init
						      nil type env))))))
       (zcprim>store-scale-slot scale old-val old-val)
       (array-initialize old-val 0)
       (zcdecl>reuse-structure old-val type env 0)
       old-val))
    
    ;; A struct gets an array of the appropriate size, built recursively from the
    ;; initializer elements or the respective defaults.
    ;; (A union gets initialized as its first variant.)
    ((zctype>struct-p type)
     (let ((old-val (or (zcdecl>old-array-check old-val type env)
			(make-array (zctype>sizeof-in-qs type env)
				    ':type 'art-q ':initial-value 0
				    ':named-structure-symbol
				    (or (zctype>struct-tag type)
					(cdr (assq (zctype>struct-class type)
						   '((:struct . struct)
						     (:packed-struct . struct)
						     (:union . union)))))
				    ':leader-list (zcprim>array-leader-init
						    nil type env)))))
       (zcprim>store-scale-slot :Q old-val old-val)
       (array-initialize old-val 0)
       (zcdecl>reuse-structure old-val type env 0)
       old-val))

    (t (ferror "Can't handle type ~A" type))))

(defun zcdecl>reuse-structure (old-val type env ielt)
  "IELT is the element of OLD-VAL to start reinitializing at; returns the index of
   the next element to do."
  (cond ((zctype>double-p type)
	 (aset #+3600 0.0d0 #-3600 0.0 old-val ielt)
	 (1+ ielt))
	((zctype>float-p type)
	 (aset #+3600 0.0 #-3600 0.0s0 old-val ielt)
	 (1+ ielt))
	((zctype>canonicalized-pointer-p type)
	 (aset nil old-val ielt)
	 (+ ielt 2))
	((zctype>function-pointer-p type)
	 (aset 'null-function-pointer old-val ielt)
	 (1+ ielt))
	((zctype>array-p type)
	 (nlet ((elt-type (zctype>pointer-deref-type type))
		(len (zctype>array-length type))
		((elt-size (zctype>scale-size (zctype>type-scale elt-type)))))
	   (if (zctype>integer-p elt-type)
	       ;; The object has already been initialized to 0s, so just skip.
	       (+ ielt (ceiling len (// 4 elt-size)))
	     (dotimes (i len)
	       (setq ielt (zcdecl>reuse-structure old-val elt-type env ielt)))
	     ielt)))
	((zctype>struct-p type)
	 (if (eq (zctype>struct-class type) ':union)
	     ;; A union gets initialized as its first element.
	     (progn (zcprim>array-as-8b old-val)
		    (zcprim>array-as-16b old-val)
		    (zcdecl>reuse-structure
		      old-val
		      (zctype>eltpr-type (car (zctype>struct-elements type env)))
		      env ielt)
		    (+ ielt (zctype>sizeof-in-qs type env)))
	   ;; Set these up so we can just do ARRAY-LEADER instructions to access.
	   (zcprim>array-as-8b old-val)
	   (zcprim>array-as-16b old-val)
	   (do ((eltprs (zctype>struct-elements type env) (cdr eltprs))
		(orig-ielt ielt))
	       ((eq (zctype>eltpr-name (car eltprs)) ':end) ielt)
	     (let ((eltpr (car eltprs)))
	       (setq ielt
		     (if (zctype>bits-p (zctype>eltpr-type eltpr))
			 (+ 1 orig-ielt (zctype>eltpr-offset eltpr))
		       (zcdecl>reuse-structure old-val (zctype>eltpr-type eltpr)
						    env ielt)))))))
	(t (1+ ielt))))

(defun zcdecl>old-array-check (old-val type env)
  "Checks to see if OLD-VAL is the right kind of array for TYPE; if so, returns
   OLD-VAL."
  (nlet ((len scale (zctype>sizeof-in-scale type env)))
    (and (arrayp old-val)
	 (eq (array-type old-val) (zcprim>scale-art scale))
	 #-Symbolics (= (array-rank old-val) 1)
	 #+Symbolics (= (array-#-dims old-val) 1)
	 (= (array-length old-val) len)
	 (array-has-leader-p old-val)
	 ( (array-leader-length old-val) (zcprim>array-leader-length))
	 old-val)))

(defun zcdecl>fill-structure (init-ptr old-val)
  "The guts of initializer processing.  This reads values from the initializer by
   way of the init-ptr and makes the appropriate structures out of them."
  (cond ((zcptr>ptr-p old-val)
	 (unless (zcdecl>init-ptr>eolp init-ptr)
	   (let ((val (eval (zcdecl>init-ptr>read++ init-ptr))))
	     (zcptr>cons (zcptr>array val) (zcptr>index val) old-val)))
	 old-val)
	((not (arrayp old-val))
	 (if (zcdecl>init-ptr>eolp init-ptr)
	     old-val
	   (eval (zcdecl>init-ptr>read++ init-ptr))))
	(t
	 (zcdecl>fill-structure-1 init-ptr old-val 0 :Q)
	 old-val)))
(defun zcdecl>fill-structure-1 (init-ptr old-val ielt scale) 
  (cond ((zcdecl>init-list-p (zcdecl>init-ptr>read init-ptr))
	 (zcdecl>init-ptr>push init-ptr)
	 (do ()
	     ((zcdecl>init-ptr>eolp init-ptr)
	      (let ((skip (zcdecl>init-ptr>where init-ptr)))
		(zcdecl>init-ptr>pop init-ptr)
		(values (+ ielt skip) old-val scale)))
	   (setf (values ielt old-val scale)
		 (zcdecl>fill-structure-1 init-ptr old-val ielt scale))))
	((not (zcdecl>init-ptr>eolp init-ptr))
	 (nlet ((init (zcdecl>init-ptr>read++ init-ptr))
		((init scale old-val ielt
		       (cond ((and (listp init) (memq (car init) '(:Q :16B :8B)))
			      (values (cadr init) (car init)
				      (zcprim>array-as-scale (car init) old-val)
				      (zctype>rescale-index ielt scale
							    (car init))))
			     ((and (listp init) (fixp (car init)))
			      (values (cddr init) (car init)
				      (zcprim>array-as-scale :Q old-val)
				      (zctype>rescale-index ielt scale :Q)))
			     (t (values init scale old-val ielt))))
		 (raw-init init)
		 ((val (eval init)))))
	   (cond ((fixp scale)               ; special case for bitfields
		  ;; (cadr raw-init) is -1 if field is in the previous word, else 0
		  (setf (ldb scale (aref old-val (+ ielt (cadr raw-init))))
			val)
		  (values (+ ielt 1 (cadr raw-init)) old-val :Q))
		 ((zcptr>ptr-p val)
		  (aset (zcptr>array val) old-val ielt)
		  (aset (zcptr>index val) old-val (1+ ielt))
		  (values (+ ielt 2) old-val scale))
		 ;; Strings get copied into OLD-VAL.
		 ((arrayp val)
		  (dotimes (i (array-active-length val))
		    (aset (aref val i) old-val (+ ielt i)))
		  (values (+ ielt (cddr raw-init)) old-val scale))
		 (t (aset val old-val ielt)
		    (values (1+ ielt) old-val scale)))))))

(defun zcdecl>initializer-compile (init-ptr type env)
  "Grovels over an initializer, checking that the lengths of all the lists match
   their respective arrays, fixing the lengths of indefinite arrays, checking
   for excessively deep nestings, and translating and type-checking expressions."
  (zcdecl>initializer-compile-1 init-ptr type env :Q t))
(defun zcdecl>initializer-compile-1 (init-ptr type env scale unpacked
				     &optional same-q-offset)     ; for bitfields.
  (cond ((zctype>void-p type)
	 (unless (zcdecl>init-ptr>eolp init-ptr)
	   (zcerror "Objects of type VOID cannot be initialized")))
	((zctype>lispval-p type)
	 (when (not (zcdecl>init-ptr>eolp init-ptr))
	   (zcdecl>init-trans-replacing init-ptr type env (and (neq scale :Q) :Q)))
	 :Q)
	((or (zctype>number-p type)
	     (and (zctype>pointer-p type)
		  (or (not (zctype>array-p type))
		      (and (zctype>char-p (zctype>pointer-deref-type type))
			   (let ((init-exp (zcdecl>init-ptr>read init-ptr)))
			     (or (zcprim>stringp init-exp) ; String might be in {}.
				 (and (zcdecl>init-list-p init-exp)
				      (zcprim>stringp (cadr init-exp)))))))))
	 (when (zctype>array-p type)
	   (nlet ((init-val (zcdecl>init-ptr>read init-ptr))
		  ((init-val (if (zcdecl>init-list-p init-val) (cadr init-val)
			       init-val))
		   ((init-str (string-to-C (cadr init-val))))))
	     (if (null (zctype>array-length type))
		 (zctype>set-array-length type (array-length init-str))
	       (if (> (array-length init-str) (zctype>array-length type))
		   (zcerror "String initializer /"~A /"~%too long for ~A array"
			    (cadr init-val) (zclstn>type-string type env))))))
	 (if (not (zcdecl>init-ptr>eolp init-ptr))
	     (let ((new-scale (zctype>type-scale type unpacked)))
	       (zcdecl>init-trans-replacing init-ptr type env
					    (and (neq new-scale scale) new-scale))
	       new-scale)
	   scale))
	((zctype>bits-p type)                ; Yuck.  Bitfields added after
	 (if (zcdecl>init-ptr>eolp init-ptr) ; compiled rep designed -- messy.
	     scale
	   (nlet ((val (zcdecl>init-trans-nested (zcdecl>init-ptr>read init-ptr)
						 (second type) env)))
	     (zcdecl>init-ptr>write++ init-ptr
				      `(,(byte (third type) (fourth type))
					,(if same-q-offset -1 0)
					. ,val))
	     :Q)))
	((and (zctype>struct-p type) (eq (zctype>struct-class type) ':union))
	 (zcdecl>initializer-compile-1
	   init-ptr (zctype>union-first-element type env) env scale nil))
	((zctype>array-p type)
	 (zcdecl>init-ptr>push init-ptr)
	 (nlet ((ar-length (zctype>array-length type)))
	   (do ((idx 0 (1+ idx))
		(elt-type (zctype>pointer-deref-type type)))
	       ((or (and ar-length (>= idx ar-length))
		    (zcdecl>init-ptr>eolp init-ptr))
		(zcdecl>excess-initializers-check init-ptr)
		(when (not ar-length)
		  (zctype>set-array-length type idx))
		(nlet ((last-list-tail (zcdecl>init-ptr>nested-p init-ptr))
		       (elt-size elt-scale (zctype>sizeof-in-scale elt-type env)))
		  ;; We stash here how many elements to skip before resuming.
		  ;; This is for the flat storage rep, so it needn't know the type
		  ;; of the thing being initialized (see zcdecl>fill-structure).
		  (when last-list-tail
		    (rplacd last-list-tail
			(zctype>rescale-index (* (- (or ar-length idx) idx)
						     elt-size)
					      elt-scale scale))))
		(zcdecl>init-ptr>pop init-ptr)
		scale)
	     (setq scale (zcdecl>initializer-compile-1 init-ptr elt-type
						       env scale nil)))))
	((zctype>struct-p type)
	 (zcdecl>init-ptr>push init-ptr)
	 (do ((eltprs (zctype>struct-elements type env) (cdr eltprs))
	      (q-offset)
	      (last-q-offset -1 q-offset)
	      (unpacked (eq (zctype>struct-class type) :struct)))
	     ((or (eq (zctype>eltpr-name (car eltprs)) ':end)
		  (zcdecl>init-ptr>eolp init-ptr))
	      (zcdecl>excess-initializers-check init-ptr)
	      (let ((last-list-tail (zcdecl>init-ptr>nested-p init-ptr)))
		;; See comments for last-list-tail above.
		(when last-list-tail
		  (rplacd last-list-tail
			  (if (eq (zctype>eltpr-name (car eltprs)) ':end) 0
			    (- (zctype>rescale-index (zctype>struct-size type env)
						     :Q scale)
			       (zctype>rescale-index
				 (zctype>eltpr-offset (car eltprs))
				 (zctype>type-scale
				   (zctype>eltpr-type (car eltprs)) unpacked)
				 scale))))))
	      (zcdecl>init-ptr>pop init-ptr)
	      scale)
	   (setq q-offset (zctype>rescale-index (zctype>eltpr-offset (car eltprs))
						(zctype>type-scale
						  (zctype>eltpr-type (car eltprs))
						  unpacked)
						:Q))
	   (setq scale (zcdecl>initializer-compile-1
			 init-ptr (zctype>eltpr-type (car eltprs))
			 env scale unpacked (= q-offset last-q-offset)))))
	(t (zcerror "Internal error: can't initialize object of type ~A"
		    type))))

(defun zcdecl>excess-initializers-check (init-ptr)
  "After some values have been read from an initializer, this checks to see if
   there are any more that shouldn't be there."
  (if (and (zcdecl>init-ptr>nested-p init-ptr)
	   (not (zcdecl>init-ptr>eolp init-ptr)))
      (zcerror "Excess initializer values: ~A" (zcdecl>init-ptr>where init-ptr))))

(defun zcdecl>init-trans-replacing (init-ptr req-type env new-scale)
  "Translates an initializer expression into Lisp, making sure its type matches
   REQ-TYPE.  Checks for redundantly nested expressions.  Replaces the expression
   with its translation, consed with NEW-SCALE if that is non-NIL."
  (nlet ((val (zcdecl>init-ptr>read init-ptr))
	 ((new-val (zcdecl>init-trans-nested val req-type env))))
    (zcdecl>init-ptr>write++
      init-ptr
      (if new-scale
	  (list* new-scale new-val    ; char array init must record desired length.
		 (and (zctype>array-p req-type)
		      (zctype>char-p (zctype>pointer-deref-type req-type))
		      (zctype>array-length req-type)))
	new-val))))

(defun zcdecl>init-trans-nested (init-exp req-type env)
  "Translates an initializer expression into Lisp, making sure its type matches
   REQ-TYPE.  Checks for redundantly nested expressions."
  (cond ((not (zcdecl>init-list-p init-exp))
	 (zcdecl>init-trans init-exp req-type env))
	((cddr init-exp)
	 (zcerror "Excess initializer values: ~A" (cddr init-exp)))
	((zcdecl>init-list-p (cadr init-exp))
	 (zcerror "Initializer nested too deeply: ~A" (cadr init-exp)))
	(t
	 (zcdecl>init-trans (cadr init-exp) req-type env))))

(defun zcdecl>init-trans (init-exp req-type env)
  "Translates an initializer expression into Lisp, making sure its type matches
   REQ-TYPE."
  (nlet ((trans-exp type (zcmac>translate-exp init-exp env nil)))
    (if (not (zctype>match type req-type env))
	(zcerror "Initializer expression ~S does not match type ~A"
		 init-exp (zclstn>type-string req-type env))
      (zcprim>coerce-numbers
	(zcprim>canonicalize-if-needed trans-exp type req-type)
	type req-type))))

; Omitted from the Symbolics system.
#+Symbolics
(defun array-initialize (a val &optional (start 0) (end (array-length a)))
  (and (> end start)
       (if (eq (array-type a) 'art-q)
	   (si:%block-store-cdr-and-contents (aloc a start) (- end start)
					     cdr-next val 0)
	 ;; This could be done faster, but it's not the common case anyhow.
	 (let ((a a))
	   (declare (sys:array-register a))
	   (do ((i start (1+ i)))
	       (( i end) a)
	     (aset val a i))))))



; ================================================================
; Miscellaneous user interface.

(defmacro undeclare (&rest variables)
  "Removes the global declaration of each of VARIABLES from the environment.
/This removes also any declarations as struct, union, or enum tags, enumeration
/constants, or typedefs."
  (mapc #'(lambda (var)
	    (remprop var 'identifier)
	    (remprop var 'definition)
	    (remprop var 'value-initializer)
	    (remprop var 'special)
	    (remprop var 'struct//union)
	    (remprop var 'enum)
	    (remprop var 'enum-constant)
	    (remprop var 'typedef)
	    (remprop var ':source-file-name))
	variables)
  `',variables)


; End of ZCTYPE.LISP
