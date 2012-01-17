(in-package #:vacietis.stdio)

(defconstant EOF -1)

(defvar *fd-lock* (bt:make-recursive-lock))

(defvar *fd-table* (make-hash-table))
(defvar *tmp-files* (make-hash-table))

(defmacro fd-stream (fd)
  `(bt:with-lock-held (*fd-lock*)
     (gethash ,fd *fd-table*)))

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

(defun register-fd (stream &optional fd)
  (bt:with-lock-held (*fd-lock*)
    (let ((fd (or fd (next-fd))))
      (setf (fd-stream fd) stream)
      fd)))

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
  (handler-case (register-fd (open-stream filename mode))
    (error () NULL)))

(defun freopen (filename mode fd)
  (handler-case (register-fd (open-stream filename mode) fd)
    (error () NULL)))

(defun fflush (fd)
  (if (= fd NULL)
      (loop for stream being the hash-value of *fd-table* do
           (when (output-stream-p stream)
             (finish-output stream)))
      (finish-output (fd-stream fd)))
  0)

(defun fclose (fd)
  (let (stream tmp-path)
    (with-lock-held (*fd-lock*)
      (setf stream (fd-stream fd)
            tmp-path (gethash fd *tmp-files*))
      (remhash fd *fd-table*)
      (remhash fd *tmp-files*))
    (close stream)
    (when tmp-path (delete-file tmp-path))
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
  (let ((path (merge-pathnames (symbol-name (gensym "vac_tmp_c_file"))
                               "/tmp/")))
    (if (open path :direction :probe)
        (tmpfile) ;; try again
        (let ((fd (fopen (string-to-char* (namestring path))
                         (string-to-char* "w+")))) ;; should be wb+
          (unless (eql fd NULL)
            (with-lock-held (*fd-lock*)
              (setf (gethash fd *tmp-files*) path)))
          ;; also need to make sure tmp files are deleted on exit
          ;; good idea to attach finalizers to the tmp files' streams too
          fd))))

(defun tmpnam (str)
  (let ((newname (string-to-char* (symbol-name (gensym)))))
   (if (eql str NULL)
       newname
       (progn (replace str newname :end1 (length newname))
              str))))

(defun setvbuf (fd buf mode size)
  (declare (ignore fd buf mode size))
  0)

;;; character I/O

(defun fgetc (fd)
  (handler-case (char-code (read-char (fd-stream fd)))
    (error () EOF)))

(defun fputc (c fd)
  (handler-case (progn (write-char (code-char c) (fd-stream fd))
                       c)
    (error () EOF)))

(defun fgets-is-dumb (str n fd replace-newline?)
  (handler-case
      (let ((stream = (fd-stream fd)))
        (loop for i from 0 below (1- n)
              for x = (read-char stream)
              do (progn (setf (aref str i) (char-code x))
                        (when (eql x #\Newline)
                          (unless replace-newline? (incf i))
                          (loop-finish)))
              finally (setf (aref str i) 0))
        str)
    (error () NULL)))

(defun fgets (str n fd)
  (fgets-is-dumb str n fd nil))

(defun gets (str)
  (fgets-is-dumb str most-positive-fixnum stdin t))

(defun fputs (str fd)
  (handler-case (progn (write-string (char*-to-string str)
                                     (fd-stream fd))
                       0)
    (error () EOF)))

(defun puts (str)
  (when (eql EOF (fputs str stdout))
    (return-from puts EOF))
  (when (eql EOF (fputc #\Newline stdout))
    (return-from puts EOF))
  0)

(defun ungetc (c fd)
  (handler-case (progn (unread-char (code-char c) fd)
                       c)
    (error () EOF)))

;;; fread/fwrite



;;; file positioning