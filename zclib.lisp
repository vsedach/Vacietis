; This file contains the portion of the ZETA-C runtime environment that's in Lisp.


; ================================================================
; Misc. conventions.

(defmacro zclib>null-pointer () '(values nil 0))
(defconst NUL 0)
(defconst EOF -1)
(defconst *SGTTYB-LENGTH* 5 "The sizeof the sgttyb structure (in sgtty.h).")

(defmacro defun-exporting (name &rest stuff)
  "Just like DEFUN, except that it also arranges that the symbol being DEFUNed is
   exported from its package."
  `(progn 'compile
     (export ',name (symbol-package ',name))
     (defun ,name . ,stuff)))

(defmacro deff-exporting (name definition)
  `(progn 'compile
     (export ',name (symbol-package ',name))
     (deff ,name ,definition)))


; ================================================================
; Terminal I/O.

(defflavor unix-terminal-io-stream
	   ((input-line nil)
	    input-index
	    (untyi-char nil)
	    (cbreak-mode nil)
	    (no-echo nil)
	    actual-stream)
	   (si:bidirectional-stream)
  (:initable-instance-variables actual-stream)
  (:gettable-instance-variables actual-stream)
  (:settable-instance-variables cbreak-mode no-echo))

(defmethod (unix-terminal-io-stream :after :init) (ignore)
  (when (not (memq *rubout-handler-message*
		   (send actual-stream :which-operations)))
    (ferror "ACTUAL-STREAM must be an interactive terminal stream; ~A isn't"
	    actual-stream)))

(defmethod (unix-terminal-io-stream :tyi) (&optional (eof-option nil))
  (cond (untyi-char
	 (prog1 untyi-char (setq untyi-char nil)))
	(cbreak-mode
	 (let ((ch (send actual-stream :tyi)))
	   (when (not no-echo) (send actual-stream :tyo ch))
	   ch))
	(t (when (or (null input-line)
		     (> input-index (string-length input-line)))
	     (nlet ((new-line eof-p end-char (readline actual-stream)))
	       (setq input-line new-line)
	       (setq input-index (if (and (zerop (string-length new-line))
					  (or eof-p (eql end-char #\End)))
				     -1 0))))
	   (let ((ch (cond ((< input-index 0)
			    eof-option)		; An END was typed.
			   ((= input-index (string-length input-line))
			    #\Return)
			   (t (aref input-line input-index)))))
	     (if (< input-index 0)
		 (setq input-line nil)
		 (incf input-index))
	     ch))))

(defmethod (unix-terminal-io-stream :untyi) (ch)
  (setq untyi-char ch))

(defmethod (unix-terminal-io-stream :clear-input) ()
  (setq input-line nil)
  (setq untyi-char nil)
  (send actual-stream :clear-input))

(defmethod (unix-terminal-io-stream :listen) ()
  (or untyi-char
      (and input-line (<= input-index (string-length input-line)))
      (send actual-stream :listen)))

(defmethod (unix-terminal-io-stream :wait-for-input-with-timeout) (timeout)
  (if (send self :listen) t
    (send actual-stream :wait-for-input-with-timeout timeout)
    (send self :listen)))

(defmethod (unix-terminal-io-stream :before :set-cbreak-mode) (on-p)
  (when on-p
    (setq input-line nil)))

(defmethod (unix-terminal-io-stream :tyo) (ch)
  (send actual-stream :tyo ch))

(defmethod (unix-terminal-io-stream :string-out) (string &optional (start 0) end)
  (send actual-stream :string-out string start end))

(defmethod (unix-terminal-io-stream :line-out) (string &optional start end)
  (send actual-stream :line-out string start end))

(defmethod (unix-terminal-io-stream :force-output) ()
  (send actual-stream :force-output))


; ================================================================
; Program startup (the "shell").

(defvar *default-pathname* nil
  "The default pathname for opening files in this process (corresponds to the Unix
   notion of a current directory).")

(defconst *stdin-fd* 0)
(defconst *stdout-fd* 1)
(defconst *stderr-fd* 2)

(defvar *file-descriptor-table*
	(let ((fdt (make-array 10.)))
	  (aset standard-input fdt *stdin-fd*)
	  (aset standard-output fdt *stdout-fd*)
	  (aset error-output fdt *stderr-fd*)
	  fdt)
  "An array associating FDs (integers) with streams.")

(defvar *signal-table* (make-array 17.)
  "An array associating signal values with functions-or-NIL.")

(defvar *terminal-stream* :unbound
  "A stream to the user's terminal, used for opening //dev//tty.")

(defun create-c-program (name)
  "Sets up a function called NAME -- in package GLOBAL: if possible, else in the
   name: package -- which is to be used as the top-level invocation function for
   a C program.  The created function takes one required argument, which is the
   current directory in which the program is to be run, followed by an &rest
   argument of strings, which are the /"command line arguments/".  It initializes
   externals, binds stdin etc. to the appropriate streams, packages up the
   /"command line arguments/" into ARGC and ARGV in the standard Unix way, and
   calls NAME:MAIN."
  (create-c-package name)
  (let ((name (intern (string name) name)))
    (unless (catch-error (globalize name) nil)
      (format error-output "~%Unable to globalize ~S~&" name))
    (eval `(defun ,name (default-pathname &rest args)
	     (zclib>run-program ',name default-pathname args)))
    (comment (load "ZETA-C: Include; stdio" :package name
		   :set-default-pathname nil))
    name))

(defmacro zclib>with-c-io (default-pathname &body body)
  "Executes BODY with the C runtime I/O environment set up.  DEFAULT-PATHNAME
   is the initial default pathname (/"current working directory/")."
  `(let ((*default-pathname* (zclib>extract-directory
			       (fs:parse-pathname ,default-pathname)))
	 (*file-descriptor-table* (make-array 10.))
	 (*terminal-stream* (make-instance 'unix-terminal-io-stream
					   :actual-stream terminal-io))
	 (*signal-table* (make-array 17.)))
     (setf (aref *file-descriptor-table* *stdin-fd*)
	   (if (terminal-io-syn-stream-p standard-input) *terminal-stream*
	     standard-input))
     (setf (aref *file-descriptor-table* *stdout-fd*)
	   (if (terminal-io-syn-stream-p standard-output) *terminal-stream*
	     standard-output))
     (setf (aref *file-descriptor-table* *stderr-fd*)
	   (if (terminal-io-syn-stream-p error-output) *terminal-stream*
	     error-output))
     (unwind-protect-case ()
	 (progn . ,body)
       (:normal (zclib>close-all-files))
       (:abort (zclib>close-all-files ':abort)))))

(defun zclib>run-program (name default-pathname args)
  "Runs a Zeta-C program NAME with arguments ARGS (a list of Lisp strings)."
  (zclib>initialize-program name)
  (nlet ((main (intern "main" name))
	 (arg-ptrs (mapcar #'string-to-C (cons (string name) args)))
	 ((argv.ary (make-array (* 2 (length arg-ptrs))))))
    (gmap nil #'(lambda (i arg) (aset arg argv.ary (* 2 i))
				(aset 0 argv.ary (+ (* 2 i) 1)))
	  (:index 0)
	  (:list arg-ptrs))
    (cond ((not (fboundp main))
	   (ferror "No /"main/" function defined for program ~A" name))
	  ((null (arglist main))
	   (when args
	     (format error-output "~S does not take arguments; ~A ignored"
		     main args))
	   (zclib>with-c-io default-pathname (funcall main)))
	  (t
	   (zclib>with-c-io default-pathname
	     (funcall main (1+ (length args)) argv.ary 0))))))

(defun zclib>initialize-program (name)
  "Initializes external and static variables in the package NAME."
  (nlet ((pkg #+Symbolics (pkg-find-package name :find)
	      #-Symbolics (find-package name))
	 ((vars (cdr (assq pkg *package-variable-initialization-alist*)))))
    (format error-output "~&Initializing externals ...")
    (mapc #'(lambda (var)
	      (let ((init-exp (get var 'value-initializer)))
		(when init-exp (set var (eval init-exp))))
	      (let ((func-init (get var 'function-initializer)))
		(when func-init (funcall func-init 'initializing))))
	  vars)
    (format error-output " done.~%")))

(defun zclib>close-all-files (&optional abort-p)
  "Closes all files opened by a C program.  If ABORT-P is non-NIL, the
   files are closed and aborted."
  (do ((i 3 (1+ i)))
      ((>= i (array-length *file-descriptor-table*)))
    (let ((stream (aref *file-descriptor-table* i)))
      (and stream (send stream :close (and abort-p ':abort))))
    (aset nil *file-descriptor-table* i)))

(defun zclib>reset-files (&optional abort-p)
  "Closes all files and reinitializes STDIN, STDOUT, and STDERR.  Use this
   instead of FS:CLOSE-ALL-FILES while debugging."
  (zclib>close-all-files abort-p)
  (nlet ((stdin (aref *file-descriptor-table* 0))
	 ((unix-terminal-io (if (and (typep stdin 'unix-terminal-io-stream)
				     (eq (send stdin :actual-stream) terminal-io))
				stdin
			      (make-instance 'unix-terminal-io-stream
					     :actual-stream terminal-io)))))
    (setf (aref *file-descriptor-table* 0) unix-terminal-io)
    (setf (aref *file-descriptor-table* 1) unix-terminal-io)
    (setf (aref *file-descriptor-table* 2) unix-terminal-io)))

; Synonym streams are SOOOO WEIRD!
(defun terminal-io-syn-stream-p (stream)
  "Is STREAM a synonym stream for TERMINAL-IO?"
  (and (symbolp stream)
       (eq (%p-contents-as-locative (function-cell-location stream))
	   #+Genera (locf terminal-io)
	   #-Genera (inhibit-style-warnings (value-cell-location 'terminal-io)))))


; ================================================================
; File I/O.

; Bidirectional I/O doesn't work in LMIT system, I think.
; OPEN etc. return an integer as file descriptor, while FOPEN etc. use NCONS of
; an integer.  The latter is intended to be stored in a pointer to FILE.
; Bidirectional I/O doesn't work in LMITI system.  Sigh: it's important for C
; programs.  Simulate with arrays?
(defun-exporting c:|open| (name-ar name-idx mode)
  "Opens the file NAME for reading (if MODE is 0), writing (if MODE is 1), or both
   (if MODE is 2).  Returns an integer file descriptor, for READ, WRITE, etc."
  (nlet ((name (string-to-lisp name-ar name-idx))
	 (options (selectq mode
		    (0 '(:direction :input))
		    (1 '(:direction :output))
		    (2 '(:direction :io #+Symbolics :direct #+Symbolics t))
		    (:otherwise (ferror "Unknown OPEN mode: ~D" mode))))
	 ((stream (lexpr-funcall #'open (zclib>merge-pathname-defaults name)
				 :error nil options))))
    (if (or (stringp stream) (errorp stream))
	-1				     ; it's an error msg; return -1
      (zclib>stream-to-fd stream))))

; Bidirectional I/O doesn't work in LMITI system.  Sigh: it's important for C
; programs.  Simulate with arrays??
(defun-exporting c:|creat| (name-ar name-idx mode)
  "Creates a file NAME.  MODE = 0: normal output file; MODE = 1: bidirectional
   direct.  Returns an integer file descriptor, for use with READ, WRITE, etc."
  (nlet ((name (string-to-lisp name-ar name-idx))
	 (options (selectq mode
		    (0 '(:direction :output))
		    (1 '(:direction :io #+Symbolics :direct #+Symbolics t))))
	 ((stream (lexpr-funcall #'open (zclib>merge-pathname-defaults name)
			:error nil options))))
    (if (or (stringp stream) (errorp stream))
	-1				     ; it's an error msg; return -1
      (zclib>stream-to-fd stream))))

(defun-exporting c:|close| (fd)
  "Closes the file on file descriptor FD."
  (let ((stream (zclib>fd-to-stream fd "CLOSE" "FCLOSE")))
    (send stream :close)
    (unless (typep stream 'unix-terminal-io-stream)
      (aset nil *file-descriptor-table* fd))
    0))

(defun-exporting c:|read| (fd buffer-ar buffer-idx nbytes)
  "Reads NBYTES from FD, putting the result in BUFFER (which should be a pointer
   into a char array."
  (nlet ((stream (zclib>fd-to-stream fd "READ" "FREAD"))
	 #+Chars (buffer-ar (zcprim>array-as-string buffer-ar))
	 ((save-array-leader-0 (and (array-has-leader-p buffer-ar)
				    (array-leader buffer-ar 0)))))
    (prog1 (- (send stream :string-in nil buffer-ar buffer-idx
		    (+ buffer-idx nbytes))
	      buffer-idx)
	   (and (array-has-leader-p buffer-ar)
		(setf (array-leader buffer-ar 0) save-array-leader-0)))))

(defun-exporting c:|write| (fd buffer-ar buffer-idx nbytes)
  "Writes NBYTES to FD from BUFFER (which should be a pointer into a char array."
  (nlet ((stream (zclib>fd-to-stream fd "WRITE" "FWRITE"))
	 #+Chars (buffer-ar (zcprim>array-as-string buffer-ar)))
    (send stream :string-out buffer-ar buffer-idx (+ buffer-idx nbytes))
    nbytes))

(defun-exporting c:|lseek| (fd offset whence)
  "Sets the read/write pointer of the file on FD to OFFSET, if WHENCE is 0; or to
   the current location plus OFFSET, if WHENCE is 1; or to the end of the file
   plus OFFSET, if WHENCE is 2.  If the resulting position is before the beginning
   of the file, does nothing and returns -1; else returns the new value of the
   pointer."
  (let ((stream (zclib>fd-to-stream fd "LSEEK" "FSEEK")))
    (when (not (memq ':set-pointer (send stream :which-operations)))
      (ferror "Stream ~A does not support seeking" stream))
    (let ((new-pos (+ offset
		      (selectq whence
			(0 0)
			(1 (send stream :read-pointer))
			(2 (send stream :length))
			(:otherwise (ferror "Unknown WHENCE option ~D" whence))))))
      (if (or (< new-pos 0) (> new-pos (send stream :length))) -1
	(send stream :set-pointer new-pos)
	new-pos))))

(defun-exporting c:|tell| (fd)
  "Returns the current value of the read/write pointer for FD."
  (send (zclib>fd-to-stream fd "TELL" "FTELL") :read-pointer))

(defun-exporting c:|isatty| (fd)
  "Is FD a stream to a terminal?"
  ;; Seems like a valid heuristic
  (if (memq *rubout-handler-message*
	    (send (zclib>fd-to-stream fd "ISATTY" "(none)") :which-operations))
      1 0))

(defun-exporting c:|gtty| (fd sgttyb-ar sgttyb-idx)
  "Gets the modes for the stream attached to FD (which must be a
   ZETA-C:UNIX-TERMINAL-IO-STREAM) into the structure pointed to by (SGTTYB-AR
   . IDX)  (#include <sgtty.h>)."
  (let ((stream (zclib>fd-to-stream fd "GTTY" "(none)")))
    (if (not (typep stream 'unix-terminal-io-stream))
	(ferror "GTTY attempted on non-terminal stream ~A (file descriptor ~D)"
		stream fd)
      (array-initialize sgttyb-ar 0 sgttyb-idx (+ sgttyb-idx *SGTTYB-LENGTH*))
      (setf (aref sgttyb-ar (+ sgttyb-idx 4))
	    (+ (if (send stream :cbreak-mode) 2 0)
	       (if (send stream :no-echo) 0 #o10)))
      0)))

(defun-exporting c:|stty| (fd sgttyb-ar sgttyb-idx &optional dont-flush)
  "Sets the modes for the stream attached to FD (which must be a
   ZETA-C:UNIX-TERMINAL-IO-STREAM) according to the structure pointed to by
   (SGTTYB-AR . IDX)  (#include <sgtty.h>)."
  (let ((stream (zclib>fd-to-stream fd "STTY" "(none)")))
    (if (not (typep stream 'unix-terminal-io-stream))
	(ferror "STTY attempted on non-terminal stream ~A (file descriptor ~D)"
		stream fd)
      (let ((flags (aref sgttyb-ar (+ sgttyb-idx 4))))
	(unless dont-flush
	  (send stream :clear-input)
	  (send stream :force-output))
	(send stream :set-cbreak-mode
	      ;; Do CBREAK for CBREAK or RAW.
	      (or (bit-test flags 2) (bit-test flags #o40)))
	(send stream :set-no-echo (not (bit-test flags #o10))))
      0)))

(defconstant TIOCGETP (deposit-byte 8 8 8 (char-code #\t)))
(defconstant TIOCSETP (deposit-byte 9 8 8 (char-code #\t)))
(defconstant TIOCSETN (deposit-byte 10 8 8 (char-code #\t)))
(defconstant FIONREAD (deposit-byte 3 8 8 (char-code #\f)))

(defun-exporting c:|ioctl| (fd opcode thing.ar thing.idx)
  "Currently handles only TIOCGETP, TIOCSETP, TIOCSETN, and FIONREAD (for
   terminals)."
  (select opcode
    (TIOCGETP (c:|gtty| fd thing.ar thing.idx))
    (TIOCSETP (c:|stty| fd thing.ar thing.idx))
    (TIOCSETN (c:|stty| fd thing.ar thing.idx t))
    ;; FIONREAD expects a pointer to int.
    (FIONREAD (zcptr>aset thing.ar thing.idx
			  (if (send (zclib>fd-to-stream fd "IOCTL" "(none)")
				    :listen)
			      1 0)))
    (:otherwise (ferror "IOCTL doesn't handle opcode #o~O" opcode))))

;;; Much prettier than hacking alarms.  Name is my idea -- does ANSI library
;;; support this?
(defun-exporting c:|ttytimeout| (fd timeout)
  (send (zclib>fd-to-stream fd "TTYTIMEOUT" "(none)")
	:wait-for-input-with-timeout timeout))

(defun-exporting c:|fopen| (name.ar name.idx mode.ar mode.idx)
  "Opens the file NAME for reading (if MODE is /"r/"), writing (if MODE is /"w/"),
   or appending (if MODE is /"a/").  Returns a stream (of type /"FILE */"), for
   use with GETC, PUTC, FPRINTF, etc."
  (nlet ((name (string-to-lisp name.ar name.idx))
	 (options (selector (string-to-lisp mode.ar mode.idx) string-equal
		    ("r" '(:direction :input))
		    ("w" '(:direction :output))
		    ("a" '(:direction :output :if-exists :append
					      :if-does-not-exist :create))
		    (:otherwise (ferror "Unknown fopen mode: ~A"
					(string-to-lisp mode.ar mode.idx)))))
	 ((stream (lexpr-funcall #'open (zclib>merge-pathname-defaults name)
				 :error nil options))))
    (if (or (stringp stream) (errorp stream))
	(zclib>null-pointer)		     ; it's an error msg; return NULL
      (zclib>fd-to-stdio (zclib>stream-to-fd stream)))))

(defun-exporting c:|fclose| (sstream.ar sstream.idx)
  "Closes the file on SSTREAM."
  (zclib>stdio-to-lispm-stream sstream.ar sstream.idx "FCLOSE" "CLOSE")
  (c:|close| (zclib>stdio-to-fd sstream.ar sstream.idx)))

(defun-exporting c:|fflush| (sstream.ar sstream.idx)
  "Flushes the output buffer on SSTREAM."
  (send (zclib>stdio-to-lispm-stream sstream.ar sstream.idx "FFLUSH")
	:force-output)
  0)

(defun-exporting c:|feof| (sstream.ar sstream.idx)
  "TRUE iff SSTREAM is at end of file."
  (if (send (zclib>stdio-to-lispm-stream sstream.ar sstream.idx "FEOF")
	    :tyipeek)
      0 1))

(defun-exporting c:|getchar| ()
  (let ((c (send (aref *file-descriptor-table* *stdin-fd*) :tyi)))
    (if c (char-code c) EOF)))

(defun-exporting c:|getc| (sstream.ar sstream.idx)
  "Returns the next character from SSTREAM, or EOF if none."
  (let ((c (send (zclib>stdio-to-lispm-stream sstream.ar sstream.idx "GETC"
					      "READ (called differently)")
		 :tyi)))
    (if c (char-code c) EOF)))

(defun-exporting c:|fgetc| (sstream.ar sstream.idx)
  "Returns the next character from SSTREAM, or EOF if none."
  (c:|getc| sstream.ar sstream.idx))

(defun-exporting c:|ungetc| (c sstream.ar sstream.idx)
  "Ungets the previously read character C from SSTREAM.  Only one character may be
   ungotten at a time, and it must be the same as the last character read."
  (unless (= c EOF)
    (send (zclib>stdio-to-lispm-stream sstream.ar sstream.idx "UNGETC") :untyi
	  (code-char c)))
  c)

(defun-exporting c:|getw| (sstream.ar sstream.idx)
  "Gets a word (two bytes) from SSTREAM.  Does not assume, nor enforce, any special
   alignment."
  (let* ((strm (zclib>stdio-to-lispm-stream sstream.ar sstream.idx "GETW"))
	 (c1 (send strm :tyi))
	 (c2 (send strm :tyi)))
    (or (and c1 c2
	     (deposit-byte (char-code c1) 8 8 (char-code c2)))
	EOF)))

(defun-exporting c:|gets| (s.ar s.idx)
  "Reads a line from STDIN into the character array S.  Does not include the
   trailing newline.  Returns NULL if end-of-file was encountered immediately."
  (let ((stdin (aref *file-descriptor-table* *stdin-fd*)))
    (if (null (send stdin :tyipeek))
	(zclib>null-pointer)
      (do ((c (send stdin :tyi) (send stdin :tyi))
	   (idx s.idx (1+ idx)))
	  ((or (null c) (eql c #\Return))
	   (aset (char-code NUL) s.ar idx)
	   (values s.ar s.idx))
	(aset (char-code c) s.ar idx)))))

(defun-exporting c:|fgets| (s.ar s.idx n sstream.ar sstream.idx)
  "Reads a line from SSTREAM into the character array S.  Reads at most N - 1
   characters before returning.  The line includes the trailing newline, if
   one was read.  Returns NULL if end-of-file was encountered immediately."
  (do ((idx s.idx (1+ idx))
       (n n (1- n))
       (strm (zclib>stdio-to-lispm-stream sstream.ar sstream.idx "FGETS")))
      ((<= n 1)
       (aset (char-code NUL) s.ar idx)
       (values s.ar s.idx))
    (let ((c (send strm :tyi)))
      (if (null c)
	  (progn (aset (char-code NUL) s.ar idx) (return (zclib>null-pointer)))
	(aset (char-code c) s.ar idx)
	(when (eql c #\Return) (setq n 1))))))

(defun-exporting c:|putchar| (ch)
  "Writes a character to the standard output."
  (tyo (code-char ch) (aref *file-descriptor-table* *stdout-fd*))
  ch)

(defun-exporting c:|putc| (ch sstream.ar sstream.idx)
  "Writes a character to SSTREAM."
  (tyo (code-char ch) (zclib>stdio-to-lispm-stream sstream.ar sstream.idx "PUTC"))
  ch)

(defun-exporting c:|fputc| (ch sstream.ar sstream.idx)
  "Writes a character to SSTREAM."
  (c:|putc| ch sstream.ar sstream.idx))

(defun-exporting c:|putw| (w sstream.ar sstream.idx)
  "Writes a word (two bytes) to SSTREAM.  Does not assume nor enforce any
   special alignment."
  (let ((strm (zclib>stdio-to-lispm-stream sstream.ar sstream.idx "PUTW")))
    (tyo (code-char (load-byte w 0 8)) strm)
    (tyo (code-char (load-byte w 8 8)) strm))
  w)

(defun-exporting c:|puts| (s.ar s.idx)
  "Writes string S to STDOUT, appending a newline."
  (send (aref *file-descriptor-table* *stdout-fd*)
	:line-out #-Chars s.ar #+Chars (zcprim>array-as-string s.ar)
	s.idx (+ s.idx (c:|strlen| s.ar s.idx)))
  '(:|No value returned from| c:|puts|))

(defun-exporting c:|fputs| (s.ar s.idx sstream.ar sstream.idx)
  "Writes string S to SSTREAM (does not append a newline)."
  (send (zclib>stdio-to-lispm-stream sstream.ar sstream.idx "FPUTS")
	:string-out #-Chars s.ar #+Chars (zcprim>array-as-string s.ar)
	s.idx (+ s.idx (c:|strlen| s.ar s.idx)))
  '(:|No value returned from| c:|fputs|))

(defun-exporting c:|fread| (buffer.ar buffer.idx item-size item-count
				      sstream.ar sstream.idx)
  "Reads ITEM-COUNT items, each ITEM-SIZE bytes long, from SSTREAM into BUFFER.
   At the moment, ITEM-SIZE must be 1 (only char arrays can be read.)"
  (zclib>stdio-to-lispm-stream sstream.ar sstream.idx "FREAD" "READ")  ; Ck error.
  (c:|read| (zclib>stdio-to-fd sstream.ar sstream.idx)
	    buffer.ar buffer.idx (* item-size item-count)))

(defun-exporting c:|fwrite| (buffer.ar buffer.idx item-size item-count
				       sstream.ar sstream.idx)
  "Writes ITEM-COUNT items, each ITEM-SIZE bytes long, to SSTREAM from BUFFER.
   At the moment, ITEM-SIZE must be 1 (only char arrays can be written.)"
  (zclib>stdio-to-lispm-stream sstream.ar sstream.idx "FWRITE" "WRITE") ; Ck error.
  (c:|write| (zclib>stdio-to-fd sstream.ar sstream.idx)
	     buffer.ar buffer.idx (* item-size item-count)))

(defun-exporting c:|fseek| (sstream.ar sstream.idx offset whence)
  "Sets the read/write pointer of SSTREAM to OFFSET, if WHENCE is 0; or to the
   current location plus OFFSET, if WHENCE is 1; or to the end of the file plus
   OFFSET, if WHENCE is 2.  If the resulting position is before the beginning of
   the file, does nothing and returns -1; else returns the new value of the
   pointer."
  (zclib>stdio-to-lispm-stream sstream.ar sstream.idx "FSEEK" "LSEEK")
  (c:|lseek| (zclib>stdio-to-fd sstream.ar sstream.idx) offset whence))

(defun-exporting c:|ftell| (sstream.ar sstream.idx)
  "Returns the current value of the read/write pointer for SSTREAM."
  (zclib>stdio-to-lispm-stream sstream.ar sstream.idx "FTELL" "TELL")
  (c:|tell| (zclib>stdio-to-fd sstream.ar sstream.idx)))

(defun-exporting c:|frewind| (sstream.ar sstream.idx)
  "Sets the read/write pointer for SSTREAM to 0."
  (zclib>stdio-to-lispm-stream sstream.ar sstream.idx "FREWIND"
			      "LSEEK (with additional arguments 0L, 0)")
  (c:|fseek| sstream.ar sstream.idx 0 0))

(defun zclib>stream-to-fd (stream)
  "Adds STREAM to the file descriptor table, and returns its index."
  (let ((idx (array-search nil *file-descriptor-table*)))
    (when (null idx)
      (setq idx (array-length *file-descriptor-table*))
      (adjust-array-size *file-descriptor-table* (* 2 idx)))
    (aset stream *file-descriptor-table* idx)
    idx))

(defun array-search (x array &optional (from 0) to)
  "Searches ARRAY for element X, using EQ for comparisons.  Search runs from
   element FROM (inclusive) to TO (exclusive); TO defaults to the active length of
   the array.  Returns the index of the first element EQ to X, or NIL if none."
  (do ((i from (1+ i))
       (to (or to (array-active-length array))))
      ((>= i to) nil)
    (when (eq x (aref array i))
      (return i))))

(defun zclib>fd-to-stream (fd func sfunc)
  (when (zcptr>ptr-p fd)
    (ferror "~A called on FOPENed stream ~S; use ~A instead"
	    func fd sfunc))
  (when (or (not (fixnump fd))
	    (< fd 0)
	    (>= fd (array-length *file-descriptor-table*)))
    (ferror "Illegal file descriptor ~S" fd))
  (or (aref *file-descriptor-table* fd)
      (ferror "File descriptor ~S refers to a closed file" fd)))

(defun zclib>stdio-to-fd (sstream.ar sstream.idx)
  (if (eq sstream.ar :stdio) sstream.idx
    sstream.ar))			     ; for back compatibility.

(defun zclib>fd-to-stdio (fd)
  (values ':stdio fd))

(defun zclib>stdio-to-lispm-stream (sstream.ar sstream.idx func &optional kfunc)
  (ignore func kfunc)
  (zclib>fd-to-stream (zclib>stdio-to-fd sstream.ar sstream.idx) nil nil))

(defmacro zclib>initialize-file-pointer (name fd)
  (multiple-value-bind (array index)
      (zcprim>pointer-var-pair name)
    `(progn 'compile ;; Why not?
	    (multiple-value (,array ,index) (zclib>fd-to-stdio ,fd))
	    (setq ,name (zcptr>cons ,array ,index)))))

(defun zclib>extract-directory (pn)
  (send pn :new-pathname :name nil :type nil :version nil))

(defun zclib>parse-pathname (name)
  (let ((pn (fs:merge-pathnames name (or *default-pathname* (fs:user-homedir)))))
    #-Symbolics				    ; Why is this necessary?  Oh well
    (when (memq (send pn :directory) '(nil :root))
      (setq pn (send pn :new-directory :unspecific)))
    pn))

(defun zclib>merge-pathname-defaults (file)
  (fs:merge-pathnames file (or *default-pathname* (fs:user-homedir))))


; ================================================================
; Misc. file operations.

(defun-exporting c:|unlink| (name.ar name.idx)
  "Deletes the file of the specified name.  Does not expunge it."
  (if (stringp (deletef (zclib>merge-pathname-defaults
			  (string-to-lisp name.ar name.idx)) nil))
      -1 0))

(defun-exporting c:|chdir| (new-dir.ar new-dir.idx)
  "Changes the current directory to that specified by NEW-DIR.  Note that NEW-DIR
   must be recognizable by FS:PARSE-PATHNAME as containing a directory."
  (setq *default-pathname*
	(zclib>extract-directory
	  (fs:parse-pathname (string-to-lisp new-dir.ar new-dir.idx))))
  0)



; ================================================================
; Printf (scanf too someday!).

(defun-exporting c:|printf| (fmt.array fmt.index &rest args)
  "Prints ARGS to STDOUT according to FMT.
   Characters in FMT are just copied to the output, except for %, which introduces
   a directive.  A directive has the following syntax:
     %[-][0][<width>][.<precision>][l]<conv>
   <conv> can be one of the following:
     d o x     The integer <arg> is printed in decimal, octal, or hex respectively.
     f         The float or double <arg> is printed in the style `[-]ddd.ddd' with
               <precision> digits after the decimal point (default 6).
     e	       The float or double <arg> is printed in the style `[-]d.ddddde[-]dd'
               with <precision> digits after the decimal point (default 6).
     g         The float or double <arg> is printed in f or e style, as appropriate
               for its magnitude.
     s	       The string <arg> is printed; if <precision> is specified, it is the
               maximum number of characters to print.
     c	       The character <arg> is printed.  NULs are ignored.
   If a minus sign appears before <width>, the value is left justified in the
   field; if a zero appears before <width>, padding will be done with zeros instead
   of blanks.  An `l' before <conv> is ignored, as is the case of <conv>."
  (zclib>printf (aref *file-descriptor-table* *stdout-fd*)
		fmt.array fmt.index args)
  '(:|No value returned from| c:|printf|))

(defun-exporting c:|fprintf| (sstream.ar sstream.idx fmt.ar fmt.idx &rest args)
  "Prints ARGS to SSTREAM according to FMT.
   Characters in FMT are just copied to the output, except for %, which introduces
   a directive.  A directive has the following syntax:
     %[-][0][<width>][.<precision>][l]<conv>
   <conv> can be one of the following:
     d o x     The integer <arg> is printed in decimal, octal, or hex respectively.
     f         The float or double <arg> is printed in the style `[-]ddd.ddd' with
               <precision> digits after the decimal point (default 6).
     e	       The float or double <arg> is printed in the style `[-]d.ddddde[-]dd'
               with <precision> digits after the decimal point (default 6).
     g         The float or double <arg> is printed in f or e style, as appropriate
               for its magnitude.
     s	       The string <arg> is printed; if <precision> is specified, it is the
	       maximum number of characters to print.
     c	       The character <arg> is printed.  NULs are ignored.
   If a minus sign appears before <width>, the value is left justified in the
   field; if a zero appears before <width>, padding will be done with zeros instead
   of blanks.  An `l' before <conv> is ignored, as is the case of <conv>."
  (zclib>printf (zclib>stdio-to-lispm-stream sstream.ar sstream.idx "FPRINTF")
	       fmt.ar fmt.idx args)
  '(:|No value returned from| c:|fprintf|))

(defun-exporting c:|sprintf| (string.ar string.idx fmt.ar fmt.idx &rest args)
  "Prints ARGS to the character array STRING according to FMT; appends a NUL at
   the end.
   Characters in FMT are just copied to the output, except for %, which introduces
   a directive.  A directive has the following syntax:
     %[-][0][<width>][.<precision>][l]<conv>
   <conv> can be one of the following:
     d o x     The integer <arg> is printed in decimal, octal, or hex respectively.
     f         The float or double <arg> is printed in the style `[-]ddd.ddd' with
               <precision> digits after the decimal point (default 6).
     e	       The float or double <arg> is printed in the style `[-]d.ddddde[-]dd'
               with <precision> digits after the decimal point (default 6).
     g         The float or double <arg> is printed in f or e style, as appropriate
               for its magnitude.
     s	       The string <arg> is printed; if <precision> is specified, it is the
	       maximum number of characters to print.
     c	       The character <arg> is printed.  NULs are ignored.
   If a minus sign appears before <width>, the value is left justified in the
   field; if a zero appears before <width>, padding will be done with zeros instead
   of blanks.  An `l' before <conv> is ignored, as is the case of <conv>."
  (labels ((stream (op &optional arg1 &rest args)
	     (selectq op
	       (:tyo
		(setf (aref string.ar string.idx) (char-code arg1))
		(incf string.idx))
	       (:which-operations '(:tyo))
	       (t (sys:stream-default-handler #'stream op arg1 args)))))
    (zclib>printf #'stream fmt.ar fmt.idx args)
    (stream :tyo NUL))
  '(:|No value returned from| c:|sprintf|))


; ================================================================
; Arithmetic.

(defun-exporting c:|abs| (val)
  "Integer absolute value."
  (abs val))

(defun-exporting c:|atoi| (str.ar str.idx)
  "Converts a string to an int.  Recognizes an optional string of tabs and spaces,
   then an optional sign, then a string of digits.  The first unrecognized character
   ends the string."
  (while (memq (aref str.ar str.idx) '(#\Space #\Tab))
    (incf str.idx))
  (zclib>read-decimal-from-string str.ar str.idx))

(defun-exporting c:|atol| (str.ar str.idx)
  "Converts a string to a long.  Recognizes an optional string of tabs and spaces,
   then an optional sign, then a string of digits.  The first unrecognized character
   ends the string."
  (c:|atoi| str.ar str.idx))

(defun-exporting c:|atof| (str.ar str.idx)
  "Converts a string to a floating-point number.  Recognizes an optional string of
   tabs and spaces, then an optional sign, then a string of digits optionally
   containing a decimal point, then an optional 'e' or 'E' followed by an
   optionally signed integer.  The first unrecognized character ends the string."
  ;; All we do is scan the string to find the end, then call SI:XR-READ-FLONUM,
  ;; which has a hairy optimized algorithm for floating-point conversion.
  (while (memq (code-char (aref str.ar str.idx)) '(#\Space #\Tab))
    (incf str.idx))
  (let ((numstart str.idx))
    (when (memq (code-char (aref str.ar str.idx)) '(#\+ #\-))
      (incf str.idx))
    (while (or (digit-char-p (code-char (aref str.ar str.idx)))
	       (char= (code-char (aref str.ar str.idx)) #\.))
      (incf str.idx))
    (when (memq (code-char (aref str.ar str.idx)) '(#\e #\E)) (incf str.idx))
    (when (memq (code-char (aref str.ar str.idx)) '(#\+ #\-)) (incf str.idx))
    (while (digit-char-p (code-char (aref str.ar str.idx)))
      (incf str.idx))
    #+Symbolics (let ((len (- str.idx numstart)))
		  (sys:with-stack-array (temp-string len :type art-string)
		    (dotimes (i len)
		      (aset (code-char (aref str.ar (+ numstart i)))
			    temp-string i))
		    (si:xr-read-flonum temp-string nil)))
    #-Symbolics (let ((temp-string (si:get-read-string))
		      (len (- str.idx numstart)))
		  (dotimes (i len)
		    (aset (aref str.ar (+ numstart i)) temp-string i))
		  (setf (fill-pointer temp-string) len)
		  (prog1 (si:xr-read-flonum temp-string nil)
			 (si:return-read-string temp-string)))))

(defun-exporting c:|fabs| (val)
  "Floating-point absolute value."
  (abs val))

(defun-exporting c:|floor| (x)
  "Returns the largest integer not greater than X."
  (float (floor x)))

(defun-exporting c:|ceil| (x)
  "Returns the smallest integer not less than X."
  (float (ceiling x)))

(defun-exporting c:|exp| (x)
  "The exponential function, base e."
  (exp x))

(defun-exporting c:|log| (x)
  "The logarithm, base e."
  (log x))

(defun-exporting c:|log10| (x)
  "The logarithm, base 10."
  (// (log x) '#,(log 10)))	  ; Want constant folding!

(defun-exporting c:|pow| (x y)
  "X to the Y power."
  (^ x y))

(defun-exporting c:|sqrt| (x)
  "Square root."
  (sqrt x))

(deff-exporting c:|sin| 'sin)

(deff-exporting c:|cos| 'cos)

(deff-exporting c:|tan| 'tan)

(deff-exporting c:|atan| 'atan2)

(deff-exporting c:|atan2| 'atan2)

(defun-exporting c:|asin| (x)
  "Arc-sine."
  (atan2 x (sqrt (- 1 (* x x)))))

(defun-exporting c:|acos| (x)
  "Arc-cosine."
  (atan2 (sqrt (- 1 (* x x))) x))

#+Symbolics
(deff-exporting c:|sinh| 'sinh)

#+TI
(deff-exporting c:|sinh| 'sinh)

;; CADR and Lambda don't seem to have this
#+MIT
(defun-exporting c:|sinh| (x)
  (let ((exp+x (exp x)))
    (* .5 (- exp+x (// 1.0 exp+x)))))

#+Symbolics
(deff-exporting c:|cosh| 'cosh)

#+TI
(deff-exporting c:|cosh| 'cosh)

#+MIT
(defun-exporting c:|cosh| (x)
  (let ((exp+x (exp x)))
    (* .5 (+ exp+x (// 1.0 exp+x)))))

#+Symbolics
(deff-exporting c:|tanh| 'tanh)

#+TI
(deff-exporting c:|tanh| 'tanh)

#+MIT
(defun-exporting c:|tanh| (x)
  (let ((exp2x (exp (* 2.0 x))))
    (// (- exp2x 1.0) (+ exp2x 1.0))))

; This isn't quite satisfactory, because it can get intermediate overflows
; even though the result is in range.
(defun-exporting c:|hypot| (x y)
  (sqrt (+ (* x x) (* y y))))


; ================================================================
; Miscellaneous.

(defun-exporting c:|longjmp| (jmp_buf val)
  "Nonlocal exit.  See SETJMP."
  (#+Symbolics throw #-Symbolics *throw jmp_buf
   (values (if (eql val 0) 1 val) jmp_buf)))

(defun-exporting c:|exit| (status)
  "Exits the program immediately.  If STATUS is nonzero, offers to enter the
   debugger.  (This works by signalling 'SYS:ABORT.)"
  (and (not (zerop status))
       (y-or-n-p
	 (format nil "Exiting with error code = ~D.~@
		      Enter the debugger (No means abort instead)? "
		 status))
       (ferror "Exiting with error code = ~D." status))
  (signal 'sys:abort #-Symbolics ""))

(defun-exporting c:|abort| ()
  (ferror "ABORT called"))


; ================================================================
; Memory allocation.

(defun-exporting c:|free| (object.ar object.idx)
  "Someday this may try to return allocated objects to a freelist.  For the
   moment, though, it does nothing."
  (ignore object.ar object.idx)
  (zclib>null-pointer))


; ================================================================
; Internal functions of various sorts.

(defun zclib>printf (stream fmt-array fmt-index args)
  ;; Still need to do %p and %n??
  (do ((fmt-index fmt-index (1+ fmt-index)))
      ((= (aref fmt-array fmt-index) (char-code NUL)) 0)
    (let ((ch (aref fmt-array fmt-index)))
      (if ( ch (char-code #\%))
	  (send stream :tyo (code-char ch))
	(incf fmt-index)
	(let ((next-idx fmt-index)
	      right-justify pad-char always+- space-flag alternate-form
	      width precision uppercase)
	  
	  ;; First we look for flags, assuming their order if present.
	  (if (= (char-code #\-) (aref fmt-array next-idx))
	      (incf next-idx)			; Skip '-'.
	    (setq right-justify t))		; The default.
	  (if ( (char-code #\0) (aref fmt-array next-idx))
	      ;; Here is where UNIX and H&S expect the '0' flag. See ** below.
	      (setq pad-char #\Space)		; Default.
	    (setq pad-char #\0) (incf next-idx))	; Skip '0'.
	  (when (= (char-code #\+) (aref fmt-array next-idx))
	    (setq always+- T) (incf next-idx))	; Skip '+'.
	  (when (= (char-code #\Space) (aref fmt-array next-idx))
	    (setq space-flag #\Space) (incf next-idx))	; Skip ' '.
	  (when (= (char-code #\#) (aref fmt-array next-idx))
	    (setq alternate-form T) (incf next-idx))	; Skip '#'.
	  
	  ;; Get width, if present.
	  (cond ((= (char-code #\*) (aref fmt-array next-idx))
		 (incf next-idx)		; Skip over the '*'
		 (setq width (pop args)))	; Consume an arg for the width.
		(T
		 (when (= (char-code #\0) (aref fmt-array next-idx))
		   ;; Here is where ANSI expects the '0' flag. See ** above.
		   (incf next-idx) (setq pad-char #\0))	; Skip '0'.
		 (multiple-value (width next-idx)
		   ;; If width is absent, 0 is 1st value returned.
		   (zclib>read-decimal-from-string fmt-array next-idx))))
	  (when (minusp width) (setq right-justify nil	; Per ANSI spec.
				     width (abs width)))
	  
	  ;; Get precision, if present.
	  (when (= (char-code #\.) (aref fmt-array next-idx))
	    (incf next-idx)			; Skip over '.'
	    (if ( (char-code #\*) (aref fmt-array next-idx))
		(multiple-value (precision next-idx)
		  ;; If width is absent, 0 is 1st value returned.
		  (zclib>read-decimal-from-string fmt-array next-idx))
	      (incf next-idx)			; Skip over '*'
	      (setq precision (pop args))))	; Consume an arg for the precision.
	  
	  (when (and precision (minusp precision))
	    (setq precision nil))		; Per ANSI spec.
	  (when (memq (code-char (aref fmt-array next-idx)) '(#\l #\L #\h))
	    (incf next-idx))			; Discard long/short info.
	  
	  (setq ch (code-char (aref fmt-array next-idx))
		fmt-index next-idx)
	  (when (eql ch (char-upcase ch))
	    (setq uppercase t			; Remember if it was uppercase.
		  ;; No int/long distinction -- let uppercase %D, %U, etc. thru.
		  ch (char-downcase ch)))
	  
	  (selectq ch
	    ((#\d #\i #\o #\x #\u)
	     (if (not (fixp (car args)))
		 (zclib>printf-error (pop args) ch stream)
	       (zclib>print-integer
		 (pop args) (or width 0) (or precision 1) pad-char
		 right-justify alternate-form uppercase always+- space-flag
		 (selectq ch ((#\d #\i #\u) 10.) (#\o 8.) (#\x 16.)) stream)))
	    ((#\e #\f #\g)
	     (if (not (floatp (car args)))
		 (zclib>printf-error (pop args) ch stream)
	       (zclib>print-flonum (pop args) (or width 0) (or precision 6)
				   pad-char right-justify alternate-form
				   uppercase always+- space-flag ch stream)))
	    (#\c
	     (if (not (fixnump (car args)))
		 (zclib>printf-error (pop args) ch stream)
	       (and (not (zerop (car args)))
		    (send stream :tyo (code-char (pop args))))))
	    (#\s
	     (nlet ((string (pop args))
		    ((start (pop args))
		     ((length (min (or precision 99999999)
				   (c:|strlen| string start))))))
	       (dotimes (i (- width length))
		 (send stream :tyo pad-char))
	       (dotimes (i length)
		 (send stream :tyo (code-char (aref string (+ start i)))))))
	    (#\a
	     (princ (pop args)))		; For lispvals: like format ~A.
	    (otherwise
	     (send stream :tyo ch))))))))

(defun zclib>printf-error (object printf-char stream)
  (if (terminal-io-syn-stream-p stream)
      (format stream "<<Attempt to print ~S with printf option %~C>>"
	      object printf-char)
    (#+Symbolics cl:cerror #-Symbolics cli:cerror
      "Use the LISP printer to print this object."
      "Attempt to print ~S with printf option %~C."
      object printf-char)
    (format stream "~S" object)))

(defun zclib>read-decimal-from-string (str idx)
  "Reads a decimal value out of a string, stopping at the first non-digit.  Returns
   the value read and the next index in the string."
  (let ((positive (selectq (code-char (aref str idx))
		      (#\+ (incf idx) t)
		      (#\- (incf idx) nil)
		      (t t))))
      (do ((ch (aref str idx) (aref str (incf idx)))
	   (val 0 (+ (* val 10.) (- ch (char-code #\0)))))
	  ((not (and (>= ch (char-code #\0)) (<= ch (char-code #\9))))
	   (values (if positive val (- val)) idx)))))

(defun zclib>print-integer (val width precision pad-char right-justify?
			    alternate-form? uppercase-hex? always+- spacep pbase
			    stream)
  "Meant to be used by ZCLIB>PRINTF."

  (unless (and (zerop val) (zerop precision))	; If PRECISION=0, don't print '0'.
    (let* ((sign (cond ((minusp val) (setq val (- val)) "-")
		       (always+- "+")
		       (spacep " ")
		       (t "")))
	   #-Symbolics
	   (si:print-fixnum-digits (if uppercase-hex?	; Kill this if ** works on LMITI.
				       "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
				     "0123456789abcdefghijklmnopqrstuvwxyz"))
	   (buffer
	     (format nil "~A~VR"
		     ;; Maybe force a leading '0' for octal, or "0x" for hex.
		     (cond ((and alternate-form? (= pbase 8)) "0")
			   ((and alternate-form? (= pbase 16))
			    (if uppercase-hex? "0X" "0x"))
			   (T ""))
		     pbase val))
	   (val-len (+ (string-length buffer) (string-length sign)))
	   (leading-0s (max 0 (- precision val-len))))
      #+Symbolics (unless uppercase-hex? (string-downcase buffer 0 nil nil))   ; **
      (when right-justify?
	(dotimes (i (- width (+ val-len leading-0s)))
	  (send stream :tyo pad-char)))
      (send stream :string-out sign)
      (dotimes (i leading-0s)			; This is how ANSI says to do this.
	(send stream :tyo #\0))
      (send stream :string-out buffer)
      (when (not right-justify?)
	(dotimes (i (- width (+ val-len leading-0s)))
	  (send stream :tyo pad-char))))))

(defun zclib>print-flonum (val width precision pad-char right-justify?
			   alternate-form? uppercase-E-format? always+- spacep
			   conv-char stream)
  "Meant to be used by ZCLIB>PRINTF.  CONV-CHAR should be one of #\e, #\f, #\g."
  ;; Can we reduce Garbage by bagging ZCLIB>PRINT-FLONUM-1, making this all one func??
  (nlet ((val negative? (if (minusp val) (values (- val) t)
			  (values val nil)))
	 ((buffer (zclib>print-flonum-1 val precision alternate-form?
					uppercase-E-format? conv-char))
	  ((val-len (+ (string-length buffer)
		       (if (or negative? always+- spacep) 1 0))))))
    (when right-justify?
      (dotimes (i (- width val-len))
	(send stream :tyo pad-char)))
    (cond (negative? (send stream :tyo #\-))
	  (always+- (send stream :tyo #\+))
	  (spacep (send stream :tyo #\Space)))
    (send stream :string-out buffer)
    (when (not right-justify?)
      (dotimes (i (- width val-len))
	(send stream :tyo pad-char)))))

(defconst print-flonum-dbg-error-msg
	  "ZETA-C internal check-- SI:SCALE-FLONUM seems to have mis-behaved in printf.
         Please send a complete bug report to ZETA-SOFT, including:
             1) The source code which excited this check, and
             2) Two screen dumps of this Debugger Frame, 
                   one now and one after typing meta-L.")

#-Symbolics
(defun zclib>print-flonum-1 (val precision alternate-form? uppercase-E-format?
			     e-format)
  "Returns the printed flonum as a string. VAL is assumed to be non-negative."
  (cond ((or (eq e-format #\e)		; We must go to Exx format.
	     (and (eq e-format #\g)
		  ;; PRECISION tells %g when to use Exx format.
		  (or (> val (^ 10 (1+ precision)))
		      (< val 1.0s-4))))

	 (nlet ((val expt (if (zerop val)
			      (values 0 0)
			    (si:scale-flonum val)))
		((buffer should-be-1		; What/why is this 2nd value??
		   (si:flonum-to-string val :ignore nil precision))))
	   ;; If ~ trailing #s, and the #\. is not required,
	   (when (and (zerop precision) (not alternate-form?))
	     (decf (fill-pointer buffer)))	; then skip it.
	   
	   ;; SI:PRINT-FLONUM moves the decimal under some conditions.  I can't
	   ;; seem to find any situation where this is needed, but I am leaving
	   ;; this in for now to check.
	   (when ( 1 should-be-1)
	     (cerror :YES :ignore :printf-dbg-jhb print-flonum-dbg-error-msg))
	   
	   (string-copy (string-append (if uppercase-E-format? "E" "e") 
				       (if (not (minusp expt)) ""
					 (setq expt (- expt)) "-")
				       ;; Exponent must have >= 2 digits.
				       (if (< (abs expt) 10) "0" "")
				       (format:output nil (princ expt)))
			buffer 0 (fill-pointer buffer))
	   buffer))
	
	(T			; Use %f whenever we aren't forced to use E-format.
	 (let ((buffer (si:flonum-to-string val :ignore nil precision)))
	   ;; If ~ trailing #s, and the #\. is not required,
	   (when (and (zerop precision) (not alternate-form?))
	     (decf (fill-pointer buffer)))	; then skip it.
	   buffer))))

#+Symbolics
(defun zclib>print-flonum-1 (val precision alternate-form? uppercase-E-format?
			     conv-char)
  (nlet ((integer exponent (si:integer-decode-float val))
	 ((round-point relative-p
	    (selectq conv-char
	      (#\g (values precision nil))
	      (#\f (values (- precision) t))
	      (#\e (values (1+ precision) nil))
	      (t (ferror "Internal error: CONV-CHAR must be one of e, f, g, not ~C"
			 conv-char))))
	  ((buffer decimal-exp ndigits
	     (si:fixed-width-decimal-digits integer exponent round-point relative-p
					    nil (make-array 16. :type art-string
							    :fill-pointer 0)))
	   ((print-exp (if (zerop ndigits) 0 (1- decimal-exp)))
	    ((e-fmt (or (char= conv-char #\e)
			(and (char= conv-char #\g)
			     (or (>= print-exp precision)
				 (< print-exp -4)))))
	     ((decpoint (if e-fmt 1 decimal-exp))))))))
    (when (zerop ndigits)
      (array-push buffer #\0)			; Must print at least one '0'.
      (setq decpoint 1))
    (while (> decpoint (fill-pointer buffer))	; When dec point is off right end,
      (array-push-extend buffer #\0))		; add trailing '0's.
    (setq ndigits (fill-pointer buffer))	; Update ndigits.
    (unless (and (char= conv-char #\g) (not alternate-form?))
      ;; Add trailing '0's after the decimal point, if any.
      (dotimes (i (max 0 (- precision (if (char= conv-char #\g) ndigits
					(- ndigits decpoint)))))
	(array-push-extend buffer #\0)))
    ;; Add leading '0's between dec point and first significant digit, if any.
    (while (< decpoint 1)
      (array-insert-extend buffer #\0 0)
      (incf decpoint))
    ;; The raw digits are in BUFFER.  Now we have to insert the decimal point
    ;; (maybe) and maybe append the exponent.
    (unless (and (= decpoint (fill-pointer buffer)) (not alternate-form?))
      (array-insert-extend buffer #\. decpoint))
    (when e-fmt
      (array-push-extend buffer (if uppercase-E-format? #\E #\e))
      (nlet ((print-exp negative? (if (minusp print-exp) (values (- print-exp) t)
				    (values print-exp nil))))
	(array-push-extend buffer (if negative? #\- #\+))
	;; And finally, print the exponent value.
	(when (< print-exp 10.)
	  (array-push-extend buffer #\0))	; Always two digits.
	(labels ((recurse (val)
		   (nlet ((quot rmdr (floor val 10.)))
		     (when (not (zerop quot))
		       (recurse quot))
		     (array-push-extend buffer
					(code-char (+ rmdr (char-code #\0)))))))
	  (recurse print-exp))))
    buffer))

#+Symbolics
(defun array-insert-extend (array val index)
  "Inserts VAL in ARRAY at INDEX, growing ARRAY if necessary to make room.
   ARRAY must have a fill pointer."
  (array-push-extend array val)	; To make room.
  (do ((i (1- (fill-pointer array)) (1- i)))
      ((= i index))
    (setf (aref array i) (aref array (1- i))))
  (setf (aref array index) val))

(defun string-to-lisp (str-ptr)
  "Converts a C character pointer STR to a Lisp string."
  (let ((nulidx (char-array-search 0 str-ptr)))
    (unless nulidx
      (error "String or character array ~a is not null-terminated." str-ptr))
    (zclib>substring str-ptr nulidx)))

(defun char-array-search (char array &optional start end)
  (let ((start (or start 0))
	(end (or end (array-active-length array)))
	(a array))
    (do ((i start (1+ i)))
	((>= i end) nil)
      (when (= char (aref a i)) (return i)))))

(defun zclib>substring (str start &optional (end (array-active-length str)))
  (nlet ((length (- end start))
	 ((newstr (make-array length :type art-string))))
    (dotimes (i length)
      (setf (aref newstr i) (code-char (aref str (+ i start)))))
    newstr))

(defun string-to-C (str)
  "Converts a Lisp string to a C character pointer (returned as two values)."
  (declare (values array index))
  (nlet ((length (string-length str))
	 ((newstr (make-array (1+ length) :type art-8b))))
    (dotimes (i length)
      (setf (aref newstr i) (char-code (aref str i))))
    (setf (aref newstr length) (char-code NUL))
    (values newstr 0)))
