(in-package #:vacietis.stdio)

(defconstant EOF -1)

(defvar *fd-table* (make-hash-table))
(defvar *fd-table-lock* (bt:make-lock))

(defmacro fd-stream (fd)
  `(gethash ,fd *fd-table*))

(defvar *next-fd* 3)

(defun next-fd ()
  (incf *next-fd*))

(defconstant stdin  0)
(defconstant stdout 1)
(defconstant stderr 2)

(setf (fd-stream stdin)  *standard-input*
      (fd-stream stdout) *standard-output*
      (fd-stream stderr) *error-output*)

;;; file operations

(defun register-fd (stream)
  (bt:with-lock-held (*fd-table-lock*)
    (let ((fd (next-fd)))
      (setf (fd-stream fd) stream)
      fd)))

(defun fopen (filename mode)
  (let ((m (char*-to-string mode))
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
    (handler-case (register-fd (apply #'open (char*-to-string filename) opts))
      (error () NULL))))

(defun freopen (filename mode fd)
  (handler-case foo
    (error () NULL)))

(defun fflush (fd)
  (if (= fd NULL)
      (loop for stream being the hash-value of *fd-table* do
           (when (output-stream-p stream)
             (finish-output stream)))
      (finish-output (fd-stream fd)))
  0)

(defun fclose (fd)
  (let (stream)
    (with-lock-held (*fd-table-lock*)
      (setf stream (fd-stream fd))
      (remhash fd *fd-table*))
    (close stream)
    0))

(defun remove (filename)
  (handler-case (progn (delete-file (char*-to-string filename))
                       0)
    (file-error () 1)))

(defun rename (oldname newname)
  (handler-case (progn (rename-file oldname newname)
                       0)
    (file-error () 1)))

(defun tmpfile ()
  (open (merge-pathnames (symbol-name (gensym "vac_tmp_c_file")) "/tmp/")
        :direction :io
        :if-does-not-exist :create
        :if-exists ))

;;; character I/O

(defun fgetc (fd)
  (handler-case (char-code (read-char (fd-stream fd)))
    (error () EOF)))

(defun fputc (c fd)
  (handler-case (write-char (code-char c) (fd-stream fd))
    (error () EOF)))

(defun fgets (str n fd)
  (handler-case (blah)
    (error () NULL)))

(defun gets (str)
  ;; read stdin until newline
  )

(defun fputs (str fd)
  (handler-case (progn blah 0)
    (error () EOF)))

(defun ungetc (c fd)
  (unread-char (code-char c) fd))
