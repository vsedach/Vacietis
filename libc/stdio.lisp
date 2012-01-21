(in-package #:vacietis.stdio)

(defconstant EOF -1)

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

(defun perror (str)
  (fprintf stderr "%s: %s\n" s "error message"))

;;; file operations

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
    (error () NULL)))

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
    (error () NULL)))

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

(defun setvbuf (fd# buf mode size)
  (declare (ignore fd# buf mode size))
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
      (let ((stream (fd-stream fd)))
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
  (handler-case (progn (write-string (char*-to-string str) (fd-stream fd))
                       0)
    (error () EOF)))

(defun puts (str)
  (when (eql EOF (fputs str stdout))
    (return-from puts EOF))
  (when (eql EOF (fputc #\Newline stdout))
    (return-from puts EOF))
  0)

(defun ungetc (c fd)
  (handler-case (progn (unread-char (code-char c) (fd-stream fd))
                       c)
    (error () EOF)))

;;; fread/fwrite

(defun fread (array obj-size num-obj fd)
  ;; todo
  )

(defun fwrite (array obj-size num-obj fd)
  ;; todo
  )

;;; file positioning

(defconstant SEEK_SET 0)
(defconstant SEEK_CUR 1)
(defconstant SEEK_END 2)

(defun fseek (fd offset origin) ;; dumbest function in stdio
  (handler-case
      (let ((stream (fd-stream fd)))
        (file-position stream (case origin
                                (0 offset)
                                (1 (+ offset (file-position stream)))
                                (2 (+ offset (file-length stream)))))
        0)
    (error () 1)))

(defun ftell (fd)
  (or (file-position (fd-stream fd)) -1))

(defun rewind (fd)
  (fseek fd 0 0)
  (clearerr fd))

(defun fgetpos (fd pos_ptr)
  (let ((pos (file-position (fd-stream fd))))
    (if pos
        (progn (setf (deref* pos_ptr) pos) 0)
        1)))

(defun fsetpos (fd pos_ptr)
  (handler-case (progn (if (file-position (fd-stream fd) (deref* pos_ptr))
                           0
                           1))
    (error () 1)))
