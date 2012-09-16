(in-package #:vacietis.vcc)

(defun del (file)
  (when (probe-file file)
    (delete-file file)))

(defun make-executable (path &key (toplevel #'main))
  (del path)
  (trivial-dump-core:save-executable path :init-function toplevel))

(defun main ()
  (let* ((c-file    (second cl-user::*command-line-argument-list*))
         (c-package (make-package (format nil "~A.PROGRAM"
                                          (string-upcase
                                           (file-namestring c-file)))
                                  :use ())))
    (let ((*package* c-package))
      (load-c-file c-file))
    (make-executable "a.out" :toplevel (lambda ()
                                         (run-c-program c-package)))))

(let ((vcc-out (merge-pathnames
                "vcc"
                #.(directory-namestring
                   (or *load-truename* *compile-file-truename*)))))
  (make-executable vcc-out))
