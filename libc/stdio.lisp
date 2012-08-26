(in-package #:vacietis.libc.stdio.h)
(in-readtable vacietis)

(define EOF -1)

;;; FILEs/streams

(defclass FILE ()
  ((stream   :initarg  :stream  :accessor fd-stream)
   (feof     :initform 0        :accessor feof)
   (ferror   :initform 0        :accessor ferror)
   (tmp-file :initform nil      :accessor tmp-file)))

(defvar stdin  (make-instance 'FILE :stream *standard-input*))
(defvar stdout (make-instance 'FILE :stream *standard-output*))
(defvar stderr (make-instance 'FILE :stream *error-output*))

(defun clearerr (fd)
  (setf (feof fd)   0
        (ferror fd) 0))

;;; file operations

;; have to do something about EEXIST
(defun open-stream (filename mode)
  (let* ((m (char*-to-string mode))
         (opts (cond ((string= m "r") '(:direction :input))
                     ((string= m "w") '(:direction :output :if-exists :overwrite
                                        :if-does-not-exist :create))
                     ((string= m "a") '(:direction :output :if-exists :append
                                        :if-does-not-exist :create))
                     ((string= m "r+") '(:direction :io))
                     ((string= m "w+") '(:direction :io :if-exists :overwrite
                                        :if-does-not-exist :create))
                     ((string= m "a+") '(:direction :io :if-exists :append
                                         :if-does-not-exist :create)))))
    (apply #'open (char*-to-string filename) opts)))

(defun fopen (filename mode)
  (handler-case (make-instance 'FILE :stream (open-stream filename mode))
    (file-error ()
      (setf errno ENOENT)
      NULL)
    (error ()
      NULL)))

(defun fflush (fd)
  (unless (eql fd NULL)
    (finish-output (fd-stream fd)))
  0)

(defun fclose (fd)
  (close (fd-stream fd))
  (when (tmp-file fd)
    (delete-file (tmp-file fd))
    (setf (tmp-file fd) nil))
  0)

(defun freopen (filename mode fd)
  (handler-case (progn (fclose fd)
                       (clearerr fd)
                       (setf (fd-stream fd) (open-stream filename mode)))
    (error ()
      (setf errno EIO)
      NULL)))

(defun remove (filename)
  (handler-case (progn (delete-file (char*-to-string filename))
                       0)
    (file-error () 1)))

(defun rename (oldname newname)
  (handler-case (progn (rename-file oldname newname)
                       0)
    (file-error () 1)))

(defun tmpfile ()
  (let ((path (merge-pathnames (symbol-name (gensym "vac_tmp_c_file"))
                               "/tmp/")))
    (if (open path :direction :probe)
        (tmpfile) ;; try again
        (let ((fd (fopen (string-to-char* (namestring path))
                         (string-to-char* "w+")))) ;; should be wb+
          (unless (eql fd NULL)
            (setf (tmp-file fd) path))
          ;; also need to make sure tmp files are deleted on exit
          ;; good idea to attach finalizers to the tmp files' streams too
          fd))))

(defun tmpnam (str)
  (let ((newname (string-to-char* (symbol-name (gensym)))))
   (if (eql str NULL)
       newname
       (progn (replace str newname :end1 (length newname))
              str))))

;;; character I/O

(defun fgetc (fd)
  (handler-case (char-code (read-char (fd-stream fd)))
    (end-of-file ()
      (setf (feof fd) 1)
      EOF)
    (error ()
      (setf (ferror fd) EIO)
      EOF)))

(defun getc (fd)
  (fgetc fd))

(defun getchar ()
  (getc stdin))

(defun fputc (c fd)
  (handler-case (progn (write-char (code-char c) (fd-stream fd))
                       c)
    (error ()
      (setf (ferror fd) EIO)
      EOF)))

(defun putc (c fd)
  (fputc c fd))

(defun putchar (c)
  (fputc c stdout))

(defun fgets-is-dumb (str n fd replace-newline?)
  (handler-case
      (let ((stream (fd-stream fd)))
        (loop for i from 0 below (1- n)
              for x = (read-char stream)
              do (progn (setf (aref str i) (char-code x))
                        (when (eql x #\Newline)
                          (unless replace-newline? (incf i))
                          (loop-finish)))
              finally (setf (aref str i) 0))
        str)
    (end-of-file ()
      (setf (feof fd) 1)
      NULL)
    (error ()
      (setf (ferror fd) EIO)
      NULL)))

(defun fgets (str n fd)
  (fgets-is-dumb str n fd nil))

(defun gets (str)
  (fgets-is-dumb str most-positive-fixnum stdin t))

(defun fputs (str fd)
  (handler-case (progn (write-string (char*-to-string str) (fd-stream fd))
                       0)
    (error ()
      (setf (ferror fd) EIO)
      EOF)))

(defun puts (str)
  (when (eql EOF (fputs str stdout))
    (return-from puts EOF))
  (when (eql EOF (fputc #\Newline stdout))
    (return-from puts EOF))
  0)

(defun ungetc (c fd)
  (handler-case (progn (unread-char (code-char c) (fd-stream fd))
                       c)
    (error ()
      (setf (ferror fd) EIO)
      EOF)))

;;; fread/fwrite, only work for byte arrays for now

(defun fread (mem element_size count fd)
  (handler-case
      (let* ((start    (memptr-ptr mem))
             (end      (+ start (* element_size count)))
             (position (read-sequence (memptr-mem mem) (fd-stream fd)
                                      :start start :end end)))
        (when (< position end)
          (setf (feof fd) 1))
        (- position start))
    (error ()
      (setf (ferror fd) EIO)
      0)))

(defun fwrite (mem element_size count fd)
  (handler-case
      (let ((start (memptr-ptr mem)))
        (write-sequence (memptr-mem mem) (fd-stream fd)
                        :start start :end (+ start (* element_size count)))
        count)
    (error ()
      (setf (ferror fd) EIO)
      0)))

;;; file positioning

(define SEEK_SET 0)
(define SEEK_CUR 1)
(define SEEK_END 2)

(defun fseek (fd offset origin) ;; dumbest function in stdio
  (handler-case
      (let ((stream (fd-stream fd)))
        (file-position stream (case origin
                                (0 offset)
                                (1 (+ offset (file-position stream)))
                                (2 (+ offset (file-length stream)))))
        (setf (feof fd) 0)
        0)
    (error ()
      (setf (ferror fd) ESPIPE) ;; is this the right error code?
      1)))

(defun ftell (fd)
  (or (file-position (fd-stream fd)) -1))

(defun rewind (fd)
  (fseek fd 0 0)
  (clearerr fd))

(defun fgetpos (fd pos_ptr)
  (let ((pos (file-position (fd-stream fd))))
    (if pos
        (progn (setf (deref* pos_ptr) pos)
               0)
        (progn (setf errno ENOTTY)
               1))))

(defun fsetpos (fd pos_ptr)
  (handler-case (progn (if (file-position (fd-stream fd) (deref* pos_ptr))
                           0
                           (progn (setf errno ESPIPE)
                                  1)))
    (error ()
      (setf errno ESPIPE) ;; is this the right code?
      1)))

;;; printf

;; adapted from ZetaC
(defun zclib>read-decimal-from-string (str idx)
  "Reads a decimal value out of a string, stopping at the first
   non-digit. Returns the value read and the next index in the
   string."
  (let ((positive (case (code-char (aref str idx))
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
  (unless (and (zerop val) (zerop precision))	; If PRECISION=0, don't print '0'
    (let* ((sign       (cond ((minusp val) (setf val (- val)) "-")
                             (always+-                        "+")
                             (spacep                          " ")
                             (t                                "")))
	   (buffer     (format nil "~A~VR"
                               (cond ((and alternate-form? (= pbase 8))
                                      "0")
                                     ((and alternate-form? (= pbase 16))
                                      (if uppercase-hex? "0X" "0x"))
                                     (t ""))
                               pbase val))
	   (val-len    (+ (length buffer) (length sign)))
	   (leading-0s (max 0 (- precision val-len))))
      (unless uppercase-hex?
        (string-downcase buffer))
      (when right-justify?
	(loop repeat (- width (+ val-len leading-0s))
              do (write-char pad-char stream)))
      (write-string sign stream)
      (loop repeat leading-0s			; This is how ANSI says to do this
            do (write-char #\0 stream))
      (write-string buffer stream)
      (unless right-justify?
	(loop repeat (- width (+ val-len leading-0s))
              do (write-char pad-char stream))))))

(defun zclib>print-flonum-1 (val precision alternate-form? uppercase-E-format?
			     e-format)
  "Returns the printed flonum as a string. VAL is assumed to be non-negative."
  (if (or (eql e-format #\e)		; We must go to Exx format.
          (and (eql e-format #\g)
               ;; PRECISION tells %g when to use Exx format.
               (or (> val (expt 10 (1+ precision)))
                   (< val 1.0e-4))))
      (format nil
              (format nil "~~,~d,2,,,,'~cE"
                      precision (if uppercase-E-format? #\E #\e))
              val)
      (format nil (format nil "~~,~dF" precision) val)))

(defun zclib>print-flonum (val width precision pad-char right-justify?
			   alternate-form? uppercase-E-format? always+- spacep
			   conv-char stream)
  "CONV-CHAR should be one of #\e, #\f, #\g"
  (let* ((negative? (minusp val))
         (val       (if negative?  (- val) val))
	 (buffer    (zclib>print-flonum-1 val precision alternate-form?
                                          uppercase-E-format? conv-char))
         (val-len   (+ (length buffer)
                       (if (or negative? always+- spacep) 1 0))))
    (when right-justify?
      (loop repeat (- width val-len)
            do (write-char pad-char stream)))
    (cond (negative? (write-char #\-     stream))
	  (always+-  (write-char #\+     stream))
	  (spacep    (write-char #\Space stream)))
    (write-string buffer stream)
    (unless right-justify?
      (loop repeat (- width val-len)
            do (write-char pad-char stream)))))

(defun fprintf (fd fmt &rest args)
  "Prints ARGS to FD according to FMT.
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
  (let ((fmt-array (memptr-mem fmt))
        (stream    (fd-stream fd)))
    (do ((fmt-index (memptr-ptr fmt) (1+ fmt-index)))
        ((= (aref fmt-array fmt-index) 0) 0)
      (let ((ch (aref fmt-array fmt-index)))
        (if (eql ch (char-code #\%))
            (let ((next-idx (incf fmt-index))
                  right-justify pad-char always+- space-flag alternate-form
                  width precision uppercase)

              ;; First we look for flags, assuming their order if present
              (if (= (char-code #\-) (aref fmt-array next-idx))
                  (incf next-idx)         ; Skip '-'
                  (setf right-justify t))
             (if (= (char-code #\0) (aref fmt-array next-idx))
                 ;; Here is where UNIX and H&S expect the '0' flag. See ** below.
                 (progn (setq pad-char #\0) (incf next-idx)) ; Skip '0'
                 (setq pad-char #\Space))
             (when (= (char-code #\+) (aref fmt-array next-idx))
               (setf always+- t) (incf next-idx)) ; Skip '+'
             (when (= (char-code #\Space) (aref fmt-array next-idx))
               (setf space-flag #\Space) (incf next-idx)) ; Skip ' '
             (when (= (char-code #\#) (aref fmt-array next-idx))
               (setf alternate-form t) (incf next-idx))	; Skip '#'

             ;; Get width, if present
             (if (= (char-code #\*) (aref fmt-array next-idx))
                 (progn
                   (incf next-idx)          ; Skip over the '*'
                   (setq width (pop args))) ; Consume an arg for the width
                 (progn
                   (when (= (char-code #\0) (aref fmt-array next-idx))
                     ;; Here is where ANSI expects the '0' flag. See ** above.
                     (incf next-idx) (setq pad-char #\0)) ; Skip '0'
                   (multiple-value-setq (width next-idx)
                     ;; If width is absent, 0 is 1st value returned
                     (zclib>read-decimal-from-string fmt-array next-idx))))

             (when (minusp width)
               (setf right-justify nil ; Per ANSI spec
                     width         (abs width)))

             ;; Get precision, if present
             (when (= (char-code #\.) (aref fmt-array next-idx))
               (incf next-idx)          ; Skip over '.'
               (if (= (char-code #\*) (aref fmt-array next-idx))
                   (progn
                     (incf next-idx)              ; Skip over '*'
                     (setf precision (pop args))) ; get arg for the precision
                   (multiple-value-setq (precision next-idx)
                     ;; If width is absent, 0 is 1st value returned
                     (zclib>read-decimal-from-string fmt-array next-idx))))

             (when (and precision (minusp precision))
               (setf precision nil))    ; Per ANSI spec
             (when (find (code-char (aref fmt-array next-idx)) "lLh")
               (incf next-idx))         ; Discard long/short info

             (let ((char (code-char (aref fmt-array next-idx))))
               (setf fmt-index next-idx)

               (when (upper-case-p char)
                 (setq uppercase t
                       ;; No int/long distinction - let uppercase %D, %U, etc. thru
                       char      (char-downcase char)))

               (case char
                 ((#\d #\i #\o #\x #\u)
                  (assert (integerp (car args)))
                  (zclib>print-integer (pop args)
                                       (or width 0)
                                       (or precision 1)
                                       pad-char
                                       right-justify
                                       alternate-form
                                       uppercase
                                       always+-
                                       space-flag
                                       (case char
                                         ((#\d #\i #\u) 10)
                                         (#\o            8)
                                         (#\x           16))
                                       stream))
                 ((#\e #\f #\g)
                  (assert (floatp (car args)))
                  (zclib>print-flonum (pop args) (or width 0) (or precision 6)
                                      pad-char right-justify alternate-form
                                      uppercase always+- space-flag ch stream))
                 (#\c
                  (unless (zerop (car args))
                    (write-char (code-char (pop args)) stream)))
                 (#\s
                  (let* ((string (pop args))
                         (length (min (or precision most-positive-fixnum)
                                      (vacietis.libc.string.h:strlen string))))
                    (loop repeat (- width length)
                          do (write-char pad-char stream))
                    (let ((str (memptr-mem string)))
                      (dotimes (i length)
                        (write-char (code-char (aref str (+ start i)))
                                    stream)))))
                 (otherwise
                  (write-char char stream)))))
            (write-char (code-char ch) stream))))))

(defun printf (fmt &rest args)
  (apply #'fprintf stdout fmt args))

(defun sprintf (str fmt &rest args)
  (replace
   (memptr-mem str)
   (memptr-mem
    (string-to-char*
     (with-output-to-string (out)
       (apply #'fprintf (make-instance 'FILE :stream out) fmt args))))
   :start1 (memptr-ptr str)))

(defun snprintf (string max-length fmt &rest args)
  (error "NOT IMPLEMENTED YET"))

(defun perror (str)
  (if (or (eql NULL str)
          (eql 0 (aref (memptr-mem str) (memptr-ptr str))))
      (fprintf stderr (string-to-char* "%s\\n") (strerror errno))
      (fprintf stderr (string-to-char* "%s: %s\\n") str (strerror errno))))

;;; things that have no effect

(defun setvbuf (fd buf mode size)
  (declare (ignore fd buf mode size))
  0)

(defun setbuf (fd buf)
  (declare (ignore fd buf))
  0)

(define FILENAME_MAX 1024)
(define FOPEN_MAX    1024)
(define BUFSIZ       512)
(define L_tmpnam     16)
(define TMP_MAX      1024)
(define _IOFBF       1)
(define _IOLBF       2)
(define _IONBF       3)
