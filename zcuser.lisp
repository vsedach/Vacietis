(defun zclstn>print (value type stream)
  "Prints a value for the C listener."
  (send stream :tyo #\()
  (zclstn>print-type type (zcenv>global-env) stream)
  (format stream ") ")
  (zclstn>print-value value type stream))

(defun zclstn>type-string (type &optional (env (zcenv>global-env)) (short-p :short))
  "Returns a string representation of TYPE in ENV.  See ZCLSTN>PRINT-TYPE re SHORT-P."
  (with-output-to-string (s) (zclstn>print-type type env s short-p)))

; Dammit, this still isn't right.  Must rethink the whole idea.  How can we tell
; when to insert parentheses?
(defun zclstn>print-type (type env stream &optional short-p (insert ""))
  "Prints a representation of TYPE (in ENV) on STREAM.  SHORT-P controls the printing
   of structure types; if NIL, the full element list is printed; if :USE-TAG, the tag
   is printed if there is one, else the full element list; if :SHORT, the tag is
   printed if any, else /"{...}/"."
  (cond ((or (zctype>number-p type) (zctype>void-p type) (zctype>lispval-p type))
	 (format stream "~(~A~)~A" type insert))
	((zctype>bits-p type)
	 (zclstn>print-type (cadr type) env stream short-p
			    (format nil "~A:~D" insert (caddr type))))
	((zctype>function-p type)
	 (zclstn>print-type (zctype>function-return-type type) env stream short-p
			    (string-append insert "()")))
	((zctype>array-p type)
	 (zclstn>print-type (zctype>pointer-deref-type type) env stream short-p
			    (format nil "~A[~@[~D~]]"
				    insert (zctype>array-length type))))
	((zctype>pointer-p type)
	 (zclstn>print-type (zctype>pointer-deref-type type) env stream :short
			    (string-append " *" insert)))
	((zctype>struct-p type)
	 (format stream "~A "
		 (cdr (assq (zctype>struct-class type)
			    '((:struct . "struct") (:packed-struct . "packed_struct")
			      (:union . "union")))))
	 (cond ((and short-p (zctype>struct-tag type))
		(format stream "~A~A" (zctype>struct-tag type)
			insert))
	       ((and short-p (neq short-p :use-tag))
		(format stream "{...}~A" insert))
	       (t
		(format stream "{ ")
		(mapc #'(lambda (pair)
			  (when (neq (zctype>eltpr-name pair) ':end)
			    (zclstn>print-type (zctype>eltpr-type pair) env stream
					       (or short-p :use-tag)
					       (format nil " ~A"
						       (zctype>eltpr-name pair)))
			    (format stream "; ")))
		      (zctype>struct-elements type env))
		(format stream "}~A" insert))))
	(t (ferror "Don't know how to print type ~A" type))))

(defvar *debug-printer-p* nil
  "Turn this on if you want to handle printer errors with the debugger.")

(defun zclstn>print-value (value type stream &optional recursive)
  (if *debug-printer-p*
      (zclstn>print-value-1 value type stream recursive)
    (nlet ((ignore error-p
	     (catch-error (zclstn>print-value-1 value type stream recursive) nil)))
;	     (zclstn>print-value-1 value type stream recursive)))
      (and error-p (format stream "<< Error printing value; trying Lisp printer: >> ~S"
			   value)))))
(defun zclstn>print-value-1 (value type stream recursive)
  (cond ((zctype>void-p type) (format stream "#{VOID}"))
	((zctype>lispval-p type) (format stream "~S" value))
	((zctype>char-p type)
	 (format stream "'~C'" value))
	((zctype>integer-p type)
	 (zclstn>print-integer value type stream recursive))
	((zctype>float-p type)
	 (format stream "~F" value))
	((zctype>array-p type)
	 (if (zctype>char-p (zctype>pointer-deref-type type))
	     (zclstn>print-string value stream 0
				  (min 60 (+ (if recursive 10 30)
					     (zctype>array-length type))))
	   (if (and recursive (neq recursive ':arrays))
	       (if (zcptr>ptr-p value)
		   (zclstn>print-element value type stream)
		 (zclstn>print-short-rep value stream))
	     (zclstn>print-array-explicit value type stream))))
	((zctype>function-pointer-p type)
	 (format stream "~A" value))
	((and (zctype>pointer-p type)
	      (zcptr>null-p (zcptr>array value) 0))
	 (format stream "NULL~:[ + ~D~]"
		 (zerop (zcptr>index value)) (zcptr>index value)))
	((zctype>pointer-p type)
	 (cond ((and recursive
		     (zclstn>fancy-print-char-pointer value type stream recursive)))
	       ((eq (zcptr>array value) ':stdio)
		;; Special hack for stdio streams.
		(format stream "Pointer to ~A, fd = ~D"
			(zclib>fd-to-stream (zcptr>index value) nil nil)
			(zcptr>index value)))
	       ((or (not (arrayp (zcptr>array value)))
		    (not (fixp (zcptr>index value))))
		(format stream "Invalid pointer with array part ~S, index part ~S"
			(zcptr>array value) (zcptr>index value)))
	       ((eq 'value-cell (named-structure-p (zcptr>array value)))
		(format stream "Pointer to ~A's value cell~:[ + ~D~]"
			(car (zcprim>array-desc (zcptr>array value)))
			(zerop (zcptr>index value))
			(// (zcptr>index value)
			    (zctype>sizeof-in-scale (zctype>pointer-deref-type type)
						    (zcenv>global-env)))))
	       (t (zclstn>print-element value type stream)))
	 (unless recursive (zclstn>fancy-print-char-pointer value type stream)))
	((zctype>struct-p type)
	 (zclstn>print-struct-explicit value type stream))
	(t (ferror "Don't know how to print value ~A of type ~A" value type))))

(defun zclstn>print-integer (value type stream recursive)
  (nlet ((infinity-relative (and (zctype>unsigned-p type) (minusp value)))
	 (len (haulong value))
	 ((octlen (max #+3600 30 #-3600 24 (+ 3 (upto-next len 3))))
	  (hexlen (max #+3600 32 #-3600 24 (+ 4 (upto-next len 4))))))
    (format stream "~:[~;~]~D" infinity-relative value)
    (when (and (not recursive) (> (abs value) 8))
      (format stream " = 0~:[~;...777...~]~O = 0x~:[~;...FFF...~]~16R"
	      infinity-relative (logand value (1- (ash 1 octlen)))
	      infinity-relative (logand value (1- (ash 1 hexlen)))))))

(defun zclstn>print-string (str stream start end &optional mark-index)
  ;; Arrays in flat mode can still be represented as consed pointers!
  (nlet ((str start end (if (arrayp str) (values str start end)
			  (values (zcptr>array str) (+ start (zcptr>index str))
				  (+ end (zcptr>index str))))))
    (send stream :tyo #\")
    (do ((i start (1+ i)))
	((or (>= i end) (>= i (array-length str))
	     (and (> i start) (= (aref-byte-safe str (1- i)) 0)
		  mark-index (< mark-index i)))
	 (when (and (< i (array-length str))
		    (not (= (aref-byte-safe str i) 0)))
	   (send stream :display-lozenged-string "...")))
      (when (and mark-index (= mark-index i))
	(send stream :display-lozenged-string "HERE->"))
      (when (< i end) (send stream :tyo (code-char (aref-byte-safe str i)))))
    (send stream :tyo #\")))

#+3600
(defun aref-byte-safe (str i)
  "Returns 0 if (aref str i) would get an /"array-word not fixnum/" error."
  (nlet ((scale-factor (// (zctype>scale-size ':Q)
			   (zctype>scale-size (zcprim>array-scale str))))
	 ((scaled-index (// i scale-factor))))
    (if (or (>= scaled-index (// (array-length str) scale-factor))
	    (fixp (aref (zcprim>array-as-q str) scaled-index)))
	(aref str i)
      0)))

#-3600
(defun aref-byte-safe (str i)
  (aref str i))

(defun zclstn>fancy-print-char-pointer (ptr type stream &optional short-p)
  "Pretty way to print a character pointer.  Can be called with any pointer; will
   do nothing if it's not either a char pointer or a pointer to storage that was
   originally declared char."
  (nlet ((array (zcptr>array ptr))
	 (index (zcptr>index ptr))
	 (dtype (zctype>pointer-deref-type type))
	 (width (if short-p 10. 30.)))
    (unless (or (not (arrayp array)) (eq (named-structure-p array) 'extern-dummy))
      (zcprim>check-array-leader array)
      (nlet ((actual (if (neq (named-structure-p array) 'cast-array) array
		       (zcprim>array-desc array)))
	     (index (* index (zctype>scale-size (zctype>type-scale dtype))))
	     ((desc (zcprim>array-desc actual))
	      ((desc (if (listp desc) desc
		       `(nil ,(zctype>array-of
				(zctype>char)
				(if (numberp desc) desc (array-length actual))))))
	       ((array (zcprim>array-as-8b actual)))
	       ((elt-type start len
		  (zclstn>element-type index :8B (cadr desc) (caddr desc)))))))
	(and (or (zctype>char-p dtype) (zctype>char-p elt-type))
	     (>= index 0) (< index (array-length array))
	     (nlet ((elt-size (zctype>scale-size (zctype>type-scale elt-type)))
		    ((start (max (* start elt-size) (- index width))))
		    ((len (* len elt-size))))
	       (progn (unless short-p (format stream "~&         i.e., "))
		      (zclstn>print-string array stream start
					   (min (+ index width) (+ start len))
					   index))))))))

(defun zclstn>element-type (index scale type env)
  "Given an index into an aggregate of type TYPE, returns the type of the scalar
   element which resides at that index.  Second value is the base of the array
   immediately containing that element, in the scale appropriate thereto, or INDEX 
   (again, rescaled) if there is no such array.  Third value is the size of that
   array (again, rescaled).  Returns NIL if the top-level aggregate is a struct, and
   the index is out of range for it."
  (nlet ((scaled-index (* index (zctype>scale-size scale)))
	 ((etype array-base array-size
	   (zclstn>element-type-1 scaled-index type env 0 0 nil t))))
    (and etype
	 (nlet ((esize (zctype>scale-size (zctype>type-scale etype))))
	   (values etype (// array-base esize)
		   (if (null array-size) 1 (// array-size esize)))))))
(defun zclstn>element-type-1 (index type env elt-base array-offset array-size top-level)
  (cond ((zctype>array-p type)
	 (nlet ((elt-type (zctype>pointer-deref-type type))
		((elt-len (zctype>sizeof elt-type env))
		 ((offset (\ index elt-len))))
		(array-size (* (or array-size 1) (zctype>sizeof type env))))
	   (zclstn>element-type-1 offset elt-type env (+ elt-base (- index offset))
				  (+ array-offset (- index offset))
				  array-size (and top-level (>= index array-size)))))
	((zctype>struct-p type)
	 ;; Unions will require knowing the last tag stored.  Later...
	 (nlet ((found elt-offset elt-type extended
		 (and (>= index 0)
		      (zctype>struct-offset-elt index :8B type env top-level))))
	   (and found (zclstn>element-type-1 (- index elt-offset) elt-type env
					     (+ elt-base elt-offset) 0 nil extended))))
	(t (values type (- elt-base array-offset)
		   (if top-level (+ array-size 99) array-size)))))

(defun zclstn>print-array-explicit (value type stream)
  "Prints a brace- and comma-delimited list of the elements of an array.  Works if
   given either a (Lisp) array or a pointer to a subarray of a structure; but the
   length given in the type must be correct."
  (if (eq (named-structure-p value) 'extern-dummy)   ; An exception.
      (zclstn>print-short-rep value stream)
    (nlet ((array (if (arrayp value) value (zcptr>array value)))
	   (length (zctype>array-length type))
	   ((desc (zcprim>array-desc array))
	    ((desc (if (arrayp desc) (zcprim>array-desc desc) desc))
	     ((env (caddr desc))))))
      (format stream "{ ")
      (do ((i 0 (1+ i))
	   (lim (min (ceiling 20 (zctype>sizeof (zctype>pointer-deref-type type) env))
		     length)))
	  ((>= i lim) (when (< i length) (format stream "...")))
	(nlet ((exp type (zcprim>translate-for-top-level
			   `(c:[] (c:quote+ ,value ,type) ,i) env)))
	  (if (and (zctype>array-p type)
		   (zctype>char-p (zctype>pointer-deref-type type)))
	      (zclstn>print-string (eval exp) stream 0
				   (min 60 (zctype>array-length type)))
	    ;; Too inflexible.  How do I print thee?  Let me count the ways...
	    (zclstn>print-value (eval exp) type stream :arrays)))
	(unless (= i (1- length))
	  (format stream ", ")))
      (format stream " }"))))

(defun zclstn>print-struct-explicit (value type stream)
  "Prints a brace- and comma-delimited list of the elements of a structure.  Works if
   given either a (Lisp) array or a pointer to a substructure of some other aggregate."
  (nlet ((array (if (arrayp value) value (zcptr>array value)))
	 ((env (zcprim>array-consed-env array))
	  ((elts (zctype>struct-elements type env)))))
    (format stream "{ ")
    (mapc #'(lambda (pair)
	      (unless (eq (zctype>eltpr-name pair) ':end)
		(nlet ((exp type (zcprim>translate-for-top-level
				   `(c:/. (c:quote+ ,value ,type) ,(car pair)) env)))
		  ;; Too inflexible.  How do I print thee?  Let me count the ways...
		  (zclstn>print-value (eval exp) type stream :arrays))
		(format stream "; ")))
	  elts)
    (format stream "}")))

(defun zclstn>print-element (pointer type stream)
  "Prints an accessor-expression representation of POINTER on STREAM."
  (zcprim>check-array-leader (zcptr>array pointer))
  (nlet ((array (zcptr>array pointer))
	 (index (zcptr>index pointer))
	 ((desc (zcprim>array-desc array))
	  (scale-size (zctype>scale-size (zcprim>array-scale array))))
	 (dtype (zctype>pointer-deref-type type)))
    (when (zctype>canonicalized-pointer-p type) (format stream "Pointer to "))
    (if (eq (named-structure-p array) 'cast-array)
	(let ((desc (zcprim>array-desc desc)))
	  (zclstn>print-short-rep array stream)
	  (nlet ((offset
		  (zclstn>print-element-1 (* index scale-size)
					  (cadr desc) (caddr desc)
					  (if (zctype>array-p type) type dtype)
					  stream t)))
	    (cond ((and (null offset) (zctype>struct-p (cadr desc)) (not (zerop index)))
		   (format stream " + ~D"
			   (// index (zctype>sizeof-in-qs dtype (caddr desc)))))
		  ((not (zerop offset))
		   (format stream ", ~A offset ~D"
			   (cdr (assq (zctype>type-scale dtype)
				      '((:Q . "word") (:16B . "halfword")
					(:8B . "byte"))))
			   (// offset
			       (zctype>scale-size (zcprim>array-scale array))))))))
      (zclstn>print-short-rep array stream)
      (nlet ((offset (zclstn>print-element-1
		       (* index scale-size) (cadr desc) (caddr desc)
		       (if (zctype>array-p type) type dtype)
		       stream t)))
	(when (and (null offset) (zctype>struct-p (cadr desc)) (not (zerop index)))
	  (format stream " + ~D"
		  (// index (zctype>sizeof-in-qs dtype (caddr desc)))))))))
(defun zclstn>print-element-1 (index type env orig-type stream top-level)
  (cond ((zctype>match type orig-type env)
	 (and (>= index 0) (< index (zctype>sizeof type env)) index))
	((zctype>array-p type)
	 (nlet ((elt-type (zctype>pointer-deref-type type))
		((elt-len (zctype>sizeof elt-type env))))
	   (format stream "[~D]" (// index elt-len))
	   (zclstn>print-element-1 (\ index elt-len) elt-type env orig-type
				   stream nil)))
	((zctype>struct-p type)
	 ;; Unions will require knowing the last tag stored.  Later...
	 (nlet ((elt elt-offset elt-type
		 (and (>= index 0)
		      (zctype>struct-offset-elt index :8B type env top-level))))
	   (if (null elt) nil
	     (format stream ".~A" elt)
	     (zclstn>print-element-1 (- index elt-offset) elt-type env orig-type
				     stream nil))))
	(t index)))

(defun zclstn>print-short-rep (object stream &optional already-started)
  "Prints a #{...} representation of OBJECT on STREAM."
  (zcprim>check-array-leader object)
  (let ((desc (zcprim>array-desc object)))
    (selectq (named-structure-p object)
      (cast-array
       (progn (format stream "#{~A view of " (array-type object))
	      (zclstn>print-short-rep desc stream t)))
      (extern-dummy
       (format stream "#{(")
       (zclstn>print-type (cadr desc) (caddr desc) stream :short)
       (format stream ") ~A, which has been declared /"extern/" but not initialized}"
	       (car desc)))
      (otherwise
       (let ((name (car desc))
	     (type (cadr desc))
	     (env (caddr desc)))
	 (unless already-started (format stream "#{"))
	 (tyo #\( stream)
	 (zclstn>print-type type env stream :short)
	 (cond ((eq (named-structure-p object) 'value-cell)
		(format stream ") ~A's value cell}" name))
	       (name (format stream ") ~A}" name))
	       (t (format stream ") ~O}" (%make-pointer dtp-fix object)))))))))

(defun zclstn>print-aggregate (ignore object stream ignore slashify)
  "General aggregate printer.  Appropriate for invocation from a NAMED-STRUCTURE-INVOKE
   property."
  (ignore slashify)                          ; For now.  What would this do?
  (zclstn>print-short-rep object stream))

;; This is just a quick hack -- better version later.
(defun zclstn>describe-aggregate (ignore object)
  (nlet ((desc (zcprim>array-desc object))
	 ((desc (if (arrayp desc) (zcprim>array-desc desc) desc))))
    (send standard-output :fresh-line)
    (zclstn>print-value object (cadr desc) standard-output)))

(defselect zclstn>aggregate-invoke
  (:print-self . zclstn>print-aggregate)
  (:describe . zclstn>describe-aggregate))

(deff (:property array named-structure-invoke) 'zclstn>aggregate-invoke)
(deff (:property struct named-structure-invoke) 'zclstn>aggregate-invoke)
(deff (:property union named-structure-invoke) 'zclstn>aggregate-invoke)
(deff (:property value-cell named-structure-invoke) 'zclstn>aggregate-invoke)
(deff (:property cast-array named-structure-invoke) 'zclstn>aggregate-invoke)
(deff (:property extern-dummy named-structure-invoke) 'zclstn>aggregate-invoke)
