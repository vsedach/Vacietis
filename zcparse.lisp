; -*- Mode: Lisp; Package: (Zeta-C Global); Base: 10 -*-
; File: ZCPARSE.LISP
;
;    This code has been placed in the public domain.
;
; This file contains the parts of the C lexer and parser that are in Lisp
; (yylex and yyparse are still in C).

; ================================================================
; The interface to the outside world: a stream whose :read message parses and
; returns top-level C expressions.

(defflavor c-parser
	   ((input-sources nil)
	    (/#if-nesting-depth 0)
	    current-directory
	    typedef-environment
	    (reading-lisp nil)
	    (/#defines-encountered nil)
	    yacc-state
	    lexer-state
	    c-package
	    source-file-symbol
	    source-file-definition-line
	    whole-file-p
	    (/#defines-expanded nil))
	   ()
  (:initable-instance-variables current-directory yacc-state c-package
				whole-file-p)
  (:gettable-instance-variables source-file-symbol whole-file-p)
  (:init-keywords :stream :pathname :start-line)
  (:required-init-keywords :stream :current-directory))

(defstruct (input-source (:type :list) :conc-name)
  stream                                ; input stream
  line-number                           ; the current line number of the stream
  string                                ; current line
  eof                                   ; have we hit eof?  or :interactive hack
  prev-line-1 prev-line-2)              ; previous two lines from this stream

(defresource input-source ()
  :constructor (make-input-source))

(defresource line-buffer ()
  :constructor (make-array 256. :type #+Chars art-8b #-Chars art-string
			   :leader-list '(0))
  :initializer (progn (aset 0 object 0) (store-array-leader 1 object 0)
		      (follow-structure-forwarding object)))

(defresource yacc-state ()
  :constructor (cparse:|yynew|))

(defresource lexer-state ()
  :constructor (cparse:|LNew|))

(defun make-c-parser (stream pathname &key start-line (c-package package)
					   (whole-file t))
  "Makes a C parser object, whose initial stream is STREAM.  PATHNAME should be a
   pathname object telling where STREAM should seem to have come from (if STREAM
   is interactive, this defines the default directory for file operations)."
  (make-instance 'c-parser :stream stream :c-package (pkg-find-package c-package)
		 :pathname pathname
		 :current-directory (zclib>extract-directory
				      (or pathname (fs:user-homedir)))
		 :start-line start-line
		 :whole-file-p whole-file))

(defmethod (c-parser :init) (plist)
  (setq yacc-state (allocate-resource 'yacc-state))
  (cparse:|yyinit| yacc-state 0 self)
  (setq lexer-state (allocate-resource 'lexer-state))
  (cparse:|LInit| lexer-state 0 self)
  (let ((top-level-stream (get plist :stream)))
    (send self :push-input-stream top-level-stream (get plist :start-line)))
  (nlet ((pathname (get plist ':pathname))
	 ((pathname-name (and pathname (send pathname :name)))))
    (setq source-file-symbol
	  (if (null pathname-name) ':interactive
	    (intern (format nil "~(~A.~A~)" pathname-name
			    (or (send pathname :type) ""))
		    (get plist :c-package)))))
  (setq whole-file-p (and whole-file-p (neq source-file-symbol ':interactive)))
  ;; We want object files to start by clearing declarations for that file.
  (when whole-file-p
    (push `(zcenv>clear-file-declarations ',source-file-symbol)
	  /#defines-encountered)))

;;; Can't believe this doesn't exist -- but I can't find it anywhere.
;;; Well, actually, looks like it might not work.  The whostate is apparently only
;;; correctly implemented for process waiting, not different varieties of running.
(defmacro with-run-state (state &body forms)
  `(let ((prevstate (process-whostate current-process)))
     (unwind-protect
	 (progn
	   (setf (process-whostate current-process) ,state)
	   (tv:who-line-run-state-update)
	   . ,forms)
       (setf (process-whostate current-process) prevstate)
       (tv:who-line-run-state-update))))

(defvar *fatal-parse-error* :unbound
  "T means we've gotten a syntax error sufficiently severe that it's not worth
   returning a form.")

(defmethod (c-parser :read) (&optional eof)
  "Reads and returns a single C top-level object from the input stream."
  (let ((source (car input-sources))
	(c-parser self))
    (if (null source) eof
      (let ((istream (input-source-stream source)))
	(if (send istream :operation-handled-p #+Symbolics :input-editor
					       #-Symbolics :rubout-handler)
	    (send istream #+Symbolics :input-editor #-Symbolics :rubout-handler
		  #-Symbolics nil #'c-parser-read-internal c-parser eof)
	  (send self :read-internal eof))))))

(defun c-parser-read-internal (c-parser eof)
  (send c-parser :read-internal eof))

(defmethod (c-parser :read-internal) (eof)
  (with-run-state "Parse C"
    (cparse:|yyreset| yacc-state 0)
    (when (and (send self :interactive)
	       ( 0 (cparse:|LAtEnd| lexer-state 0)))
      (send self :reset-interactive-input))
    (setq typedef-environment (zcenv>global-env))
    (setq source-file-definition-line nil)
    (unwind-protect-case ()
	(or (pop /#defines-encountered)
	    (nlet ((*fatal-parse-error* nil)
		   ((form (*catch 'parse-accepted
			    (cparse:|yyparse| yacc-state 0)))))
	      (and (not *fatal-parse-error*)
		   (or form (pop /#defines-encountered) eof))))
      (:normal (setq /#defines-expanded nil))	     ; Forget macro expansions.
      (:abort (send self :reset-interactive-input)))))

(defmethod (c-parser :reset-interactive-input) ()
  "Call this to reset things if appropriate before reading a form from the keyboard."
  (setf (fill-pointer (input-source-string (car input-sources))) 0)
  (cparse:|LReset| lexer-state 0))

;; This seems to be much the cleanest way to implement Lisp escapes.
;; This is the one the Symbolics system checks for...
(defwhopper (c-parser :operation-handled-p) (op)
  (if (eq op :read) (not reading-lisp)
    (continue-whopper op)))

;; ... and the LMITI system checks this one.
(defwhopper (c-parser :which-operations) ()
  (let ((ops (continue-whopper)))
    (if reading-lisp (remq :read ops 1) ops)))

;; Also apparently necessary, sometimes (??).
(defmethod (c-parser :interactive) ()
  (eq (input-source-eof (car input-sources)) ':interactive))

(defmethod (c-parser :pathname) ()
  (and (not (send self :interactive))
       (send (input-source-stream (car (last input-sources))) :pathname)))


; For porting ZETA-C.

(defun parse-file (infile outfile)
  "C-parses INFILE, writing the resulting list structure to OUTFILE."
  (let ((infile (fs:parse-pathname infile))
	(outfile (fs:merge-pathname-defaults outfile infile)))
    (with-open-file (instream infile)
      (let ((gp (send instream #+Symbolics :generic-pathname-and-plist
		      #-Symbolics :generic-pathname)))
	(fs:read-attribute-list gp instream)
	(with-open-file (outstream outfile :direction :output)
	  (let ((base 10.)
		#-Genera (*nopoint nil)
		#+Genera (cl:*print-radix* t)
		(package (pkg-find-package (send gp :get :package)))
		(prinlevel nil)
		(prinlength nil))
	    (with-open-stream (c-stream (make-c-parser instream infile))
	      (format outstream "; -*- Mode: Lisp; Package: ~A; Base: 10 -*-~@
				 ; File: ~A.~A~%;~%;   Copyright (C) 1985 by ZETA-SOFT, Ltd.~@
				 ; All rights reserved.~%;~%"
		      package (send outfile :name) (send outfile :type))
	      (do ((form (send c-stream :read nil) (send c-stream :read nil)))
		  ((null form))
		(let ((si:print-readably t))
		  (format outstream "~S~%" form))))))))))

; To see what the Lisp output looks like.
(defun translate-file (infile &optional outfile)
  "Translates a C source file, INFILE, into Lisp, grinding the result to OUTFILE."
  (nlet ((infile (fs:parse-pathname infile))
	 ((outfile (if outfile (fs:merge-pathname-defaults outfile infile)
		     (send infile :new-canonical-type ':lisp)))))
    (with-open-file (instream infile)
      (let ((gp (send instream #+Symbolics :generic-pathname-and-plist
		      #-Symbolics :generic-pathname)))
	(fs:read-attribute-list gp instream)
	(with-open-file (outstream outfile :direction :output)
	  (let ((base 10.)
		#-Genera (*nopoint nil)
		#+Genera (cl:*print-radix* t)
		(package (pkg-find-package (send gp :get :package)))
		(prinlevel nil)
		(prinlength nil))
	    (with-open-stream (c-stream (make-c-parser instream infile))
	      (format outstream "; -*- Mode: Lisp; Package: ~A; Base: 10 -*-~%~%"
		      package)
	      (do ((form (send c-stream :read nil) (send c-stream :read nil))
		   (si:print-readably t))
		  ((null form))
		(si:grind-top-level (macroexpand form) nil outstream)))))))))

; For patches in C mode (and possibly other uses).
(defmacro c-source (pkg file line str)
  "Expands to the parse of STR, parsed in package PKG with info recorded as if STR
   had come from line LINE in FILE (a printed pathname)."
  `(progn 'compile . ,(parse-string pkg file line str)))

(defun parse-string (pkg file line str)
  "C-parses STR in package PKG; records info as if this source had come from line
   LINE in FILE (a printed pathname).  Returns a list of forms."
  (with-input-from-string (instream str)
    (with-open-stream (c-stream
			(make-c-parser instream (and file (fs:parse-pathname file))
				       :start-line line :whole-file nil
				       :c-package (pkg-find-package pkg)))
      (do ((form (send c-stream :read nil) (send c-stream :read nil))
	   (forms nil (cons form forms)))
	  ((null form) (nreverse forms))))))


; ================================================================
; Support routines for the lexer.

;;; Back compatibility to Symbolics Rel 6, TI Rel 2.
#-Chars
(progn (defsubst char-code (c) c)
       (defsubst code-char (c) c)
       (defsubst char= (c1 c2) (= c1 c2))
       (defsubst char< (c1 c2) (< c1 c2)))

(defconstant *ascii-NUL* 0)

;;; Called by cparse:|LInt| in ZCTOKEN.C.
#+Symbolics
(defsubst si:xr-read-fixnum-internal (string ii len &optional (ibs ibase))
  (si:xr-read-integer-internal string ii len ibs))

;;; Misnamed -- actually used for operator names.
(defmacro cparse:|LMisc| (name idx)
  `',(intern (string-upcase (string-constant name idx)) 'c))

(defmacro cparse:|LWhere| (c-parser)
  `(send ,c-parser :where))

(defmethod (c-parser :where) ()
  (if (eq source-file-symbol ':interactive)
      nil
    (let ((top-level-source (car (last input-sources))))
      (list source-file-symbol
	    (or source-file-definition-line
		(input-source-line-number top-level-source))
	    (typep (input-source-stream top-level-source)
		   'zwei:interval-stream)))))

(defmacro cparse:|LError| (c-parser error-point message &rest args)
  `(send ,c-parser :lex-error ,error-point ,message . ,args))

(defmethod (c-parser :lex-error) (error-point message &rest args)
  (let ((source (car input-sources))
	;; This would otherwise just be bound to the C-PARSER instance, which
	;; is boring to print out.
	#-Symbolics (si:read-stream nil))
    (if (null source)
	(sys:parse-ferror "Unexpected end of input stream; ~@
			   The following message may or may not be applicable:~%~?"
			  message (copylist args))
      (nlet ((stream (input-source-stream source))
	     (error-loc (min (string-length (input-source-string source))
			     (+ (cparse:|LBufIndex| lexer-state 0) error-point)))
	     #-Symbolics ((compiler:warn-on-errors-stream stream)))
	(cond ((eq (input-source-eof source) ':interactive)
	       (sys:parse-ferror "Error while tokenizing: ~?" message (copylist args)))
	      ((typep stream 'zwei:interval-stream)
	       (zclex>zmacs-error stream error-loc message (copylist args)))
	      (t (sys:parse-ferror "Error while tokenizing line ~D~@[ of ~A~]:~%~?~@
				    Error location:~%~AHERE~A"
				   (input-source-line-number source)
				   (send stream :send-if-handles :truename)
				   message (copylist args)
				   (zclib>substring (input-source-string source)
						    0 error-loc)
				   (zclib>substring (input-source-string source)
						    error-loc))))))))

(defun zclex>zmacs-error (stream error-loc message args)
  "Called when a lexer or syntax error is encountered in input being read from a
   Zmacs buffer.  We've been reading entire lines from the interval stream, so in
   order for Zmacs to correctly set *point* to the location of the error, we have
   to back up the stream to there."
  (let ((now-bp (send stream :read-bp)))
    (send stream :set-bp (zwei:create-bp (zwei:line-previous (zwei:bp-line now-bp))
					 error-loc))
    #-Symbolics (lexpr-funcall #'sys:parse-ferror message args)
    ;; Strictly speaking, we should be calling SYS:PARSE-FERROR here too.  However,
    ;; Symbolics Zmacs does not bind the right handlers for that (lose, lose).
    #+Symbolics (lexpr-funcall #'sys:read-error stream message args)))

(defprop sys:parse-ferror t :error-reporter)

(defmacro cparse:|LWarn| (c-parser error-point message &rest args)
  `(send ,c-parser :lex-warn ,error-point ,message . ,args))

(defmethod (c-parser :lex-warn) (error-point message &rest args)
  "A lexer warning.  These mostly have to do with portability."
  (nlet ((source (car input-sources))
	 ((error-loc (min (string-length (input-source-string source))
			  (+ (cparse:|LBufIndex| lexer-state 0) error-point)))))
    (format error-output "~&~?~%Context:~%~AHERE~A"
	    message args
	    (zclib>substring (input-source-string source) 0 error-loc)
	    (zclib>substring (input-source-string source) error-loc))))

(defun cparse:|LWarnNoCtx| (message &rest args)
  (send error-output :fresh-line)
  (lexpr-funcall #'format error-output message args))

;;; Support routine for the support routines.
(defun string-constant (ary &optional (idx 0))
  "Given a string constant as returned by the lexer, returns a normal Lisp
   string constant.  Back compatible with Zetalisp strings.  This is meant to
   be called only from macros in the lexer & parser back end."
  (flet ((invalid () (ferror "Internal error: invalid string constant")))
    (unless (eql idx 0)
      (invalid))
    (when (and (listp ary) (eq (car ary) 'quote))
      (setq ary (cadr ary)))
    (cond ((listp ary)
	   (unless (eq (car ary) 'c:string+)
	     (invalid))
	   (cadr ary))
	  ((not (arrayp ary))
	   (invalid))
	  ((eq (array-type ary) 'art-8b)
	   (let ((lstr (make-array (or (char-array-search 0 ary)
				       (array-active-length ary))
				   :type art-string)))
	     (dotimes (i (array-length lstr))
	       (aset (code-char (aref ary i)) lstr i))
	     lstr))
	  ((eq (array-type ary) 'art-string)
	   (if (null (string-search #\  ary)) ary
	     (let ((lstr (make-array (string-search #\  ary) :type art-string)))
	       (dotimes (i (array-length lstr))
		 (aset (aref ary i) lstr i))
	       lstr)))
	  (t (invalid)))))


; ================================================================
; Support routines for the parser.

(defvar *debug-lexer-stream* nil)

(defmacro cparse:|yylex| (c-parser)
  `(send ,c-parser :yylex))

(defmethod (c-parser :yylex) ()
  (nlet ((lexeme (cparse:|LToken| lexer-state 0))
	 ((yylval (cparse:|Lyylval| lexer-state 0))))
    (and *debug-lexer-stream*
	 (format *debug-lexer-stream* "~D: ~A   " lexeme yylval))
    (when (null source-file-definition-line)
      (setq source-file-definition-line
	    (input-source-line-number (car (last input-sources)))))
    (cparse:|yysetlval| yylval yacc-state 0)
    lexeme))

(defsubst cparse:|PCons| (car cdr)
  (cons-in-area car cdr zc-temporary-area))

(defsubst cparse:|PCar| (cons)
  (car cons))

(defsubst cparse:|PCdr| (cons)
  (cdr cons))

(defsubst cparse:|PNil| ()
  ())

(defsubst cparse:|PList| (&rest frobs)
  (apply #'list frobs))

(defsubst cparse:|PReverse| (list)
  (nreverse list))

(defsubst cparse:|PAppend| (ls1 ls2)
  (nconc ls1 ls2))

(defun cparse:|PStringCat| (s1 s2)
  (unless (and (listp s1) (eq (car s1) 'c:string+)
	       (listp s2) (eq (car s2) 'c:string+))
    (ferror "Internal error: string constants in wrong format"))
  `(c:string+ ,(string-append (cadr s1) (cadr s2))))

; The only special-case back end routine for the parser!
(defun cparse:|PIncrForm| (op arg preorpost)
  (list (or (cdr (assq op (cdr (assq preorpost 'c:((1 . ((++ . ++x) (-- . --x)))
						   (2 . ((++ . x++) (-- . x--))))))))
	    (ferror "Internal error: bad call to PIncrForm"))
	arg))

(defmacro cparse:|PDefinedp| (c-parser sym)
  `(if (send ,c-parser :definedp ,sym) 1 0))

(defmacro cparse:|PPushEnv| (c-parser)
  `(send ,c-parser :push-env))

(defmethod (c-parser :push-env) ()
  "Adds a frame to the current environment."
  (setq typedef-environment (zcenv>create-env typedef-environment)))

(defmacro cparse:|PPopEnv| (c-parser)
  `(send ,c-parser :pop-env))

(defmethod (c-parser :pop-env) ()
  "Removes a frame from the current environment."
  (setq typedef-environment (zcenv>parent typedef-environment)))

(defmacro cparse:|PDeclare| (c-parser decl)
  `(send ,c-parser :declare ,decl))

(defmethod (c-parser :declare) (decl)
  "Processes DECL in the current environment, in case it defines a typedef."
  (let ((*source-location* (send self :where)))
    (zcdecl>parser-declaration decl typedef-environment)))

(defsubst cparse:|PAccept| (result)
  (*throw 'parse-accepted result))

(defmacro cparse:|PError| (c-parser msg.ary msg.idx &optional (fatal nil fatalp))
  `(send ,c-parser :parse-error ,msg.ary ,msg.idx . ,(and fatalp `(,fatal))))

(defmethod (c-parser :parse-error) (msg.ary msg.idx &optional fatal)
  (nlet ((source (car input-sources))
	 ((stream (input-source-stream source)))
	 ;; These would otherwise just be bound to the C-PARSER instance, which
	 ;; is boring to print out.
	 #-Symbolics ((si:read-stream nil)
		      ((compiler:warn-on-errors-stream stream)))
	 ((error-point (min (string-length (input-source-string source))
			    (cparse:|LBufIndex| lexer-state 0)))))
    (when fatal (setq *fatal-parse-error* t))
    (cond ((eq (input-source-eof source) ':interactive)
	   (sys:parse-ferror "Error while parsing: ~A" (string-to-lisp msg.ary msg.idx)))
	  ((typep stream 'zwei:interval-stream)
	   (zclex>zmacs-error stream error-point (string-to-lisp msg.ary msg.idx) nil))
	  (t (sys:parse-ferror
	       "Error while parsing line ~D~@[ of ~A~]:~%~A~@
		Error happened somewhere before the point indicated by /"HERE/" in:~@
		~A~A~A HERE ~A"
	       (input-source-line-number source)
	       (send stream :send-if-handles :truename)
	       (string-constant msg.ary msg.idx)
	       (string-to-lisp (input-source-prev-line-2 source) 0)
	       (string-to-lisp (input-source-prev-line-1 source) 0)
	       (zclib>substring (input-source-string source) 0 error-point)
	       (string-to-lisp (input-source-string source) error-point))))))

(defmacro cparse:|yyerror| (c-parser msg.ary msg.idx)
  (and (not (and (listp msg.ary) (eq (car msg.ary) 'quote)
		 (array-compare (cadr msg.ary) (string-to-C "syntax error"))))
       `(cparse:|PError| ,c-parser ,msg.ary ,msg.idx)))


; ================================================================
; The preprocessor and misc. support stuff.

(defconst zclex>*typedef-type-lexeme* 264.
  "The YACC-assigned lexeme value for typedef'ed names.")

(defconst zclex>*misc-symbol-lexeme* 280.
  "The YACC-assigned lexeme value for miscellaneous symbols.")

(defconst zclex>*defined-op-lexeme* 314.
  "The YACC-assigned lexeme value for the /"defined/" operator.")

(defconst zclex>*lisp-inclusion-lexeme* 315.
  "The YACC-assigned lexeme value for Lisp inclusions.")

; Sigh -- this list has to be manually updated when the parser changes.
(defconst zclex>*reserved-word-lexemes*
	  'c:((|auto| . 261)
	      (|break| . 275)
	      (|case| . 274)
	      (|char| . 262)
	      (|continue| . 276)
	      (|default| . 279)
	      (|do| . 271)
	      (|double| . 262)
	      (|else| . 269)
	      (|enum| . 267)
	      (|extern| . 261)
	      (|float| . 262)
	      (|for| . 272)
	      (|goto| . 278)
	      (|if| . 268)
	      (|int| . 262)
	      (|lispval| . 262)
	      (|long| . 263)
	      (|optarg| . 261)
	      (|packed_struct| . 266)
	      (|register| . 261)
	      (|restarg| . 261)
	      (|return| . 277)
	      (|short| . 263)
	      (|signed| . 263)
	      (|sizeof| . 304)
	      (|static| . 261)
	      (|struct| . 266)
	      (|switch| . 273)
	      (|typedef| . 261)
	      (|union| . 266)
	      (|unsigned| . 263)
	      (|void| . 262)
	      (|while| . 270)))

(defconst IFPARSE 2                ; see "#define IFPARSE" in zctoken.c
  "The value of IFSTATE that indicates a #if expression is being parsed;
   enables the /"defined/" operator.")

(defconst IFDEFINED 3              ; see "#define IFDEFINED" in zctoken.c
  "The value of IFSTATE that indicates that this symbol is the argument
   to the /"defined/" operator.")

(defmethod (c-parser :process-symbol) (ifstate nomacro name)
  "Interns a symbol, checking to see if it is defined as a macro."
  (nlet ((case-symbol (zclex>intern name c-package (and *case-insensitive* :soft)))
	 ((defn (and case-symbol
		     (not (= ifstate IFDEFINED))
		     (= nomacro 0)
		     (or (cdr (assq case-symbol /#defines-expanded))
			 (zcenv>/#definition case-symbol (send self :where)))))))
    (if defn
	(progn				     ; Once an expansion has been selected by
	  (when (send self :interactive)     ; menu, we want to keep it till next :read
	    (push (cons case-symbol defn) /#defines-expanded))
	  (cparse:|LSymbol1| lexer-state 0 0 case-symbol (car defn) (cdr defn)))
      (nlet ((symbol (if (not *case-insensitive*) case-symbol
		       (zclex>intern name c-package :upcase)))
	     (case-symbol-lexeme (cdr (assq case-symbol
					    zclex>*reserved-word-lexemes*)))
	     ((lexeme (cond (case-symbol-lexeme)
			    ((and (= ifstate IFPARSE)
				  (string-equal case-symbol "defined"))
			     zclex>*defined-op-lexeme*)
			    ((= ifstate IFDEFINED) zclex>*misc-symbol-lexeme*)
			    ((and (zcenv>typedef-type symbol typedef-environment)
				  (let ((*source-location* (send self :where))
					(zcenv>*attempt-substitution* nil))
				    (zcenv>typedef-type symbol typedef-environment)))
			     zclex>*typedef-type-lexeme*)
			    (t zclex>*misc-symbol-lexeme*)))))
	(cparse:|LSymbol1| lexer-state 0 lexeme
			   (if case-symbol-lexeme case-symbol
			     ;; Special case to make "GL$NIL" work right (sigh --
			     ;; only needed because ZCMAC>TRANSLATE-EXP is bogus).
			     (or symbol 'c:(/#lisp ((|lispval|)) global:nil))))))))

(defun zclex>intern (name c-package &rest options)
  "Interns a symbol in the current package.  Meaningful OPTIONS include :UPCASE,
   which causes conversion to upper case, and :SOFT, which causes the symbol not
   to be interned if not present.  '$' is interpreted as the package prefix
   delimiter."
  (when (memq :upcase options)
    (dotimes (i (string-length name))
      (aset (char-upcase (aref name i)) name i)))
  (nlet (($pos (string-search-char #/$ name))
	 ((pkg
	    (and $pos (pkg-find-package (string-upcase (nsubstring name 0 $pos))
					:find c-package)))
	  ((pkg name (if (and pkg (neq pkg c-package))
			 (values pkg (nsubstring name (1+ $pos)))
		       (values c-package name))))))
    (if (memq :soft options)
      (intern-soft name pkg)
    (intern name pkg))))

(defmethod (c-parser :intern) (name &rest options)
  (lexpr-funcall #'zclex>intern name c-package options))

(defmethod (c-parser :define) (symbol args defn)
  (let ((do-it-form (zcenv>#define symbol (cons args defn) (send self :where))))
    (unless (send self :interactive)
      (tail-push do-it-form /#defines-encountered))))

(defmethod (c-parser :undef) (symbol)
  (let ((do-it-form (zcenv>#define symbol nil (send self :where))))
    (unless (send self :interactive)
      (tail-push do-it-form /#defines-encountered))))

(defmethod (c-parser :definedp) (symbol)
  (zcenv>#definition symbol (send self :where)))

(defmethod (c-parser :if) ()
  (using-resource (ystate yacc-state)
    (cparse:|yyreset| ystate 0)
    (#+Genera sys:%with-binding-stack-level
     #-Genera progn
       (#+Symbolics sys:%bind-location
	#-Symbolics bind (locf yacc-state) ystate)	; LET doesn't work on
       (nlet ((*fatal-parse-error* nil)			; instance vars
	      ((form (*catch 'parse-accepted (cparse:|yyparse| ystate 0)))))
	 (and (not *fatal-parse-error*)
	      ( (zcprim>eval-constant-int form) 0))))))

(defmethod (c-parser :include) (filename where)
  "Set up a file inclusion."
  (let ((current-pathname
	  (fs:merge-pathname-defaults filename current-directory :h))
	(include-pathname
	  (fs:merge-pathname-defaults filename *system-include-directory* :h))
	(include-sys-pathname
	  (fs:merge-pathname-defaults filename *system-include-sys-directory* :h)))
    (send self :push-input-stream
	  (if (and (eq where :current) (probef current-pathname))
	      (open current-pathname)
	    (if (memq where '(:current :include))
		(open include-pathname)
	      (open include-sys-pathname))))))

(defmethod (c-parser :current-file) ()
  (send (input-source-stream (car input-sources)) :send-if-handles :pathname))

(defmethod (c-parser :current-line) ()
  (if (eq source-file-symbol ':interactive) 0
    (input-source-line-number (car (last input-sources)))))

(defmethod (c-parser :comments-nest-p) ()
  nil)                             ; for now.

(defmethod (c-parser :readlisp) ()
  (unwind-protect
      (let ((readtable #+Symbolics si:*common-lisp-readtable*   ; Yeesh.
		       #-Symbolics si:common-lisp-readtable)
	    (package c-package))
	(setq reading-lisp t)
	(read self))
    (setq reading-lisp nil)))

;; Used by LMacroSymbol.
(defun array-compare (ar1 ar2 &optional (start1 0) (start2 0) end1 end2)
  "Compare portions of two arrays, using EQL.  Defaults like STRING-EQUAL."
  (let ((len1 (- (or end1 (array-active-length ar1)) start1))
	(len2 (- (or end2 (array-active-length ar2)) start2)))
  (and (= len1 len2)
       (do ((idx1 start1 (1+ idx1))
	    (idx2 start2 (1+ idx2))
	    (count len1 (1- count)))
	   ((zerop count) t)
	 (when (neql (aref ar1 idx1) (aref ar2 idx2))
	   (return nil))))))


; ================================================================
; Low level input code.  This maintains a stack of input sources.

;; Reads a line (or possibly just a character or two) for the lexer.
;; Returns an array containing the characters, NUL-terminated.
;; Returns NIL when no more input from this source.
(defmethod (c-parser :next-line) ()
  (nlet ((source (car input-sources)))
    (cond ((null source)
	   (cparse:|LSetBuf| lexer-state 0 nil 0))
	  ((eq (input-source-eof source) ':interactive)
	   (send self :kbd-next-char))
	  ((input-source-eof source)
	   (cparse:|LSetBuf| lexer-state 0 nil 0))
	  (t (send self :file-next-line)))))

(defmethod (c-parser :file-next-line) ()
  (let ((source (car input-sources)))
    (swapf (input-source-prev-line-1 source) (input-source-prev-line-2 source))
    (string-copy (input-source-string source) (input-source-prev-line-1 source))
    (labels ((get-line (at-idx)
	       (nlet ((new-line eof-p (send (input-source-stream source) :line-in))
		      ((length (array-active-length new-line))))
		 (incf (input-source-line-number source))
		 (string-copy new-line (input-source-string source) 0 at-idx)
		 (array-push-extend (input-source-string source)
				    (char-code #\Return))
		 (array-push-extend (input-source-string source) *ascii-NUL*)
		 (if (or eof-p (zerop length)
			 (not (char= #/\ (aref new-line (- length 1)))))
		     (setf (input-source-eof source) eof-p)
		   (get-line (+ at-idx length -1))))))
      (get-line 0)
;     (setf (input-source-string source)     ; sigh, confuses the resource manager
;           (follow-structure-forwarding (input-source-string source)))
      (cparse:|LSetBuf| lexer-state 0 (input-source-string source) 0))))

;; We read keyboard input by single characters.
;; This accumulates each line of input in the buffer, though currently
;; nothing is done with the lines; someday we may save them for source-
;; level debugging.  The macro-expander assumes they're being saved,
;; however, so don't dike this out.
(defmethod (c-parser :kbd-next-char) ()
  (nlet ((source (car input-sources))
	 ((buffer (input-source-string source))
	  ((fill (fill-pointer buffer))))
	 (c (send self :tyi-interactive)))
    (when (and (plusp fill) (char= (code-char (aref buffer (1- fill))) #\Return))
      ;; Here's where we would save the line if we were doing so.
      (setf (fill-pointer buffer) 0))
    (array-push-extend buffer (char-code c))
    (array-push-extend buffer *ascii-NUL*)
    (decf (fill-pointer buffer))
    (cparse:|LSetBuf| lexer-state 0 buffer (1- (fill-pointer buffer)))
    (when (char= c #/\)
      (let ((c (send self :tyi-interactive)))
	(array-push-extend buffer (char-code c))
	(array-push-extend buffer *ascii-NUL*)
	(decf (fill-pointer buffer))
	(when (char= c #\Return)
	  (send self :kbd-next-char))))))    ; Skip over \<Return>; read next.

(defmethod (c-parser :tyi-interactive) ()
  (nlet ((source (car input-sources))
	 ((c (send (input-source-stream source) :tyi))))
    (when (and (not reading-lisp)
	       (or (char< c #\Space)
		   (and (> (char-code c) 127.)
			(not (mem #'char= c '(#\Return #\Tab #\Page))))))
      (send self :lex-error -1 "Illegal character"))
    c))

;; Called when reading a Lisp form.
(defmethod (c-parser :any-tyi) (&optional eof-action)
  (let ((c (cparse:|LTyi| lexer-state 0)))
    (if (zerop c)
	(and eof-action (signal 'sys:end-of-file))
      (code-char c))))

;; Or maybe this one is called, depending on the system (which is which?).
(defmethod (c-parser :tyi) (&optional eof-action)
  (send self :any-tyi eof-action))

;; This too.
(defmethod (c-parser :untyi) (char)
  (cparse:|LUnTyi| lexer-state 0 (char-code char)))

; I'm surprised this doesn't exist.
(defun string-copy (from to &optional (from-start 0) (to-start 0) from-end to-end)
  "Copy the contents of string FROM to string TO.  TO must have a fill pointer,
   which gets set to the end of the copied data.  TO will be grown if necessary.
   The -START and -END arguments default like STRING-EQUAL's.  FROM and TO may be
   either ART-STRING or ART-8B."
  ;; More trivial incompatibilities.  Yuck.
  (check-arg to (and (arrayp to) (memq (array-type to) '(art-string art-8b))
		     (array-has-leader-p to) (fixp (fill-pointer to)))
	     #+Symbolics 'string-with-fill-pointer
	     #-Symbolics "a string with a fill pointer"
	     #-Symbolics string-with-fill-pointer)
  (nlet ((from-length (- (or from-end (array-active-length from)) from-start))
	 (to-length (and to-end (- to-end to-start)))
	 ((nchars (if to-length (min to-length from-length) from-length))
	  ((to-new-length (+ to-start nchars))))
	 (delta (if (and (eq from to) (< from-start to-start)) -1 1))
	 (to-allocated-size #+Symbolics (array-dimension-n 1 to)
			   #-Symbolics (array-dimension to 0))
	 (string-to-8b (and (eq (array-type from) 'art-string)
			    (eq (array-type to) 'art-8b)))
	 (8b-to-string (and (eq (array-type from) 'art-8b)
			    (eq (array-type to) 'art-string))))
    (when (< to-allocated-size to-new-length)
      (adjust-array-size to (fix (* 1.5 to-new-length))))
    (setf (fill-pointer to) to-new-length)
    (do ((from-i (if (plusp delta) from-start (+ from-start nchars -1))
		 (+ from-i delta))
	 (to-i (if (plusp delta) to-start (+ to-start nchars -1)) (+ to-i delta))
	 (nchars nchars (1- nchars))
	 #+3600 (from from)
	 #+3600 (to to))
	((zerop nchars))
      #+3600 (declare (sys:array-register from to))
      (aset (cond (string-to-8b (char-code (aref from from-i)))
		  (8b-to-string (code-char (aref from from-i)))
		  (t (aref from from-i)))
	    to to-i))))

(defmethod (c-parser :push-input-stream) (stream &optional start-line)
  (let ((is (allocate-resource 'input-source)))
    (cparse:|LSetBuf| lexer-state 0 nil 0)
    ;; More trivial incompatibilities.  Yeesh!
    #+Symbolics (alter-input-source is stream stream
				    line-number (1- (or start-line 1))
				    string (allocate-resource 'line-buffer)
				    eof (and (memq *rubout-handler-message*
						   (send stream :which-operations))
					     :interactive)
				    prev-line-1 (allocate-resource 'line-buffer)
				    prev-line-2 (allocate-resource 'line-buffer))
    #-Symbolics (alter-input-source is :stream stream
				    :line-number (1- (or start-line 1))
				    :string (allocate-resource 'line-buffer)
				    :eof (and (memq *rubout-handler-message*
						    (send stream :which-operations))
					      :interactive)
				    :prev-line-1 (allocate-resource 'line-buffer)
				    :prev-line-2 (allocate-resource 'line-buffer))
    (push is input-sources)))

(defmethod (c-parser :pop-input-source) ()
  "Pops an input source from the stack.  Returns non-NIL if there are more sources
   to be read from."
  (if (null (cdr input-sources))
      ;; If there's only one source left, we don't actually pop it, but return NIL
      ;; as if we did (so PError can intelligibly report unexpected-EOF).
      nil
    (send self :close-input-source (pop input-sources))
    ;; Similarly, if the top-level source is interactive, we must have done a #include
    ;; in the listener; to make things happen neatly, fake EOF here.
    (and (not (send self :interactive))
	 input-sources)))

(defmethod (c-parser :close-input-source) (source)
  (send (input-source-stream source) :close)
  (deallocate-resource 'line-buffer (input-source-string source))
  (deallocate-resource 'line-buffer (input-source-prev-line-1 source))
  (deallocate-resource 'line-buffer (input-source-prev-line-2 source))
  (deallocate-resource 'input-source source)
  (cparse:|LSetBuf| lexer-state 0 nil 0)
  input-sources)

(defmethod (c-parser :close) (&optional ignore)
  (unless (eq input-sources ':this-c-parser-stream-closed-already)
    (while input-sources (send self :close-input-source (pop input-sources)))
    (deallocate-resource 'lexer-state lexer-state)
    (deallocate-resource 'yacc-state yacc-state)
    (setq input-sources ':this-c-parser-stream-closed-already)))

;;; Used to close interactively-invoked #includes.
(defmethod (c-parser :close-includes) ()
  (while (cdr input-sources) (send self :close-input-source (pop input-sources))))


(compile-flavor-methods c-parser)


; End of ZCPARSE.LISP
