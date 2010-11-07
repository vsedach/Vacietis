;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for Private version 0.0
;;; Reason: Function ZWEI:COMPILE-INTERVAL:  Sends :READ-INTERVAL-STREAM to major
;;;   mode to support parsing streams for other languages.  Also, uses
;;;   WITH-OPEN-STREAM to be sure the stream is closed after use.
;;; Function ZWEI:INDENT-BP-ADJUSTMENT:  This used to work only on (point).
;;; Written by Gyro, 5/30/88 03:14:34
;;; while running on Dionysos from FEP0:>Genera-7-2-Dionysos.load.1
;;; with Genera 7.2, Experimental Logical Pathnames Translation Files NEWEST,
;;; ZETA-C (c) 1987 ZETA-SOFT, Ltd. 21.2, microcode 3620-MIC 420, FEP 206,
;;; Fep0:>g206-lisp.flod(21), Fep0:>g206-loaders.flod(21), Fep0:>g206-info.flod(21),
;;; Fep0:>g206-debug.flod(8), Machine serial number 20340.


;(NOTE-PRIVATE-PATCH "Patches to Zmacs required to support ZETA-C.")


;=====================================
(SYSTEM-INTERNALS:BEGIN-PATCH-SECTION)
(SYSTEM-INTERNALS:PATCH-SECTION-SOURCE-FILE "SYS:ZWEI;COMC.LISP.331")
(SYSTEM-INTERNALS:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Zetalisp; Base: 8;Mode: LISP; Package: ZWEI-*-")

;;; COMPILE-P is T to call the compiler, in which case COMPILE-PROCESSING-MODE will
;;; be the compiler used.  Otherwise, COMPILE-P is NIL (meaning EVAL-PRINT) or a
;;; function.  That function is called on the forms read.  Note that in this case
;;; the forms are not sent through the compiler at all, so that a -*-Eval-*- property
;;; will not be screwed by calling the regular macroexpander.
(DEFUN COMPILE-INTERVAL (COMPILE-P *USE-TYPEOUT* SI:*REDO-DEFVARS-P*
			 BP1 &OPTIONAL BP2 IN-ORDER-P
			 SPECIAL-COMPILER-FUNCTION
			 (COMPILER:*READ-THEN-PROCESS-FLAG* COMPILER:*READ-THEN-PROCESS-FLAG*)
			 &AUX (STANDARD-OUTPUT *TYPEOUT-WINDOW*)
			 (STANDARD-INPUT *TYPEOUT-WINDOW*))
  (DECLARE (SPECIAL *USE-TYPEOUT*))
  (GET-INTERVAL BP1 BP2 IN-ORDER-P)
  (CHECK-INTERVAL-SECTIONS BP1 BP2 T)
  (LET* ((BUFFER (BP-BUFFER BP1))
	 (GENERIC-PATHNAME (LET ((PATHNAME (AND (FILE-BUFFER-P BUFFER)
						(SEND BUFFER :PATHNAME))))
			     (AND PATHNAME (SEND PATHNAME ':GENERIC-PATHNAME))))
	 (MAJOR-MODE (SEND BUFFER ':MAJOR-MODE))
	 (COMPILER-FUNCTION (OR SPECIAL-COMPILER-FUNCTION
				(SEND MAJOR-MODE ':COMPILER-FUNCTION)))
	 (ADDITIONAL-BINDINGS (LOOP WITH LIST = (SEND MAJOR-MODE ':ADDITIONAL-ATTRIBUTES)
				    FOR (INDICATOR VALUE)
					ON (SEND BUFFER ':SEND-IF-HANDLES ':PLIST) BY 'CDDR
				    WHEN (ASSQ INDICATOR LIST)
				      COLLECT INDICATOR AND COLLECT VALUE)))
    (with-open-stream (STREAM (or (send major-mode :send-if-handles
					:read-interval-stream bp1 bp2 t)
				  (OPEN-INTERVAL-STREAM BP1 BP2 T)))
      (MOVE-POINT-ON-READ-ERROR (STREAM)
	(IF (EQ COMPILE-P T)
	    (IF (SEND MAJOR-MODE ':COMPILATION-SUPPORTED)
		;; Any properties from the buffer for package and base should override
		;; those in the generic pathname.  COMPILE-FROM-STREAM does the bindings
		(LET ((INHIBIT-FDEFINE-WARNINGS INHIBIT-FDEFINE-WARNINGS))
		  (COMPILER:COMPILE-FROM-STREAM STREAM
						GENERIC-PATHNAME
						COMPILER-FUNCTION ADDITIONAL-BINDINGS))
	      ;; Otherwise, compilation is not supported, so give an error
	      (BARF "The major mode ~A does not support compilation"
		    (SEND MAJOR-MODE ':MAJOR-MODE-KEYWORD)))
	  ;; Else evaluation, if supported
	  (IF (SEND MAJOR-MODE ':EVALUATION-SUPPORTED)
	      ;; Bind all the variables required by the attribute list.
	      (MULTIPLE-VALUE-BIND (FVARS FVALS)
		  (FS:FILE-ATTRIBUTE-BINDINGS
		    GENERIC-PATHNAME :ADDITIONAL-ATTRIBUTES ADDITIONAL-BINDINGS)
		(STANDARD-VALUE-PROGV FVARS FVALS
		  (LET ((SYS:FDEFINE-FILE-PATHNAME GENERIC-PATHNAME)
			(SI:PATCH-SOURCE-FILE-PATHNAME NIL)
			(COMPILER:*COMPILE-FUNCTION* COMPILER-FUNCTION)
			(INHIBIT-FDEFINE-WARNINGS INHIBIT-FDEFINE-WARNINGS))
		    (COMPILER:PROCESS-FORMS-FROM-STREAM
		      STREAM
		      (OR COMPILE-P
			  (SEND MAJOR-MODE ':EVAL-PRINT-FUNCTION))))))
	    ;; Otherwise, evaluation is not supported, so give an error
	    (BARF "The major mode ~A does not support evaluation"
		  (SEND MAJOR-MODE ':MAJOR-MODE-KEYWORD)))))
      (CLOSE STREAM))))


;=====================================
(SYSTEM-INTERNALS:BEGIN-PATCH-SECTION)
(SYSTEM-INTERNALS:PATCH-SECTION-SOURCE-FILE "SYS:ZWEI;INDENT.LISP.161")
(SYSTEM-INTERNALS:PATCH-SECTION-ATTRIBUTES
  "-*- Base: 8;Mode: LISP;Package: ZWEI-*-")

;; If the point is within this line's indentation, move it past that
;; indentation.
(DEFUN INDENT-BP-ADJUSTMENT (BP)
  (LET ((BP1 (FORWARD-OVER *BLANKS* (CREATE-BP (BP-LINE BP) 0))))
    (COND ((AND (< (BP-INDEX bp)
		   (BP-INDEX BP1)))
	   (MOVE-BP BP BP1)))))

