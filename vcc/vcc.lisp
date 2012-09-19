(in-package #:vacietis.vcc)

#-(or sbcl ccl clisp) (error "VCC only supports CCL, SBCL, and CLISP at this time.")

(defun del (file)
  (when (probe-file file)
    (delete-file file)))

(defun getargv ()
  #+sbcl   sb-ext:*posix-argv*
  #+ccl    ccl:*command-line-argument-list*
  #+cmucl  *command-line-strings*
  #+clisp  (ext:argv)
  #+ecl    (ext:command-args))

(defun make-executable (path &key (toplevel #'main))
  (del path)
  (trivial-dump-core:save-executable path toplevel))

(defun main ()
  (let* ((c-file     (second (getargv)))
         (c-package  (find-package '#:vacietis.vcc.c-program))
         (*package*  c-package))
    (load-c-file c-file)
    (make-executable "a.out" :toplevel (lambda ()
                                         (run-c-program c-package)))))

(let ((vcc-out (merge-pathnames
                "vcc"
                #.(directory-namestring
                   (or *load-truename* *compile-file-truename*)))))
  (make-executable vcc-out))
