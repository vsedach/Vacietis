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

(defun printf (fmt &rest args)
  (apply #'fprintf stdout fmt args))

(defun fprintf (fd fmt &rest args)
  )

(defun sprintf (str fmt &rest args)
  (replace
   (memptr-mem str)
   (memptr-mem
    (string-to-char*
     (with-output-to-string (out)
       (apply #'fprintf (make-instance 'FILE :stream out) fmt args))))
   :start1 (memptr-ptr str)))

(defun snprintf (string max-length fmt &rest args))

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
