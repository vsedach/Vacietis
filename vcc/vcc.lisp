(in-package #:vacietis.vcc)

#-ccl (error "VCC only supports Clozure Common Lisp for now.")

(defun del (file)
  (when (probe-file file)
    (delete-file file)))

(defun main ()
  (let* ((c-file    (second cl-user::*command-line-argument-list*))
         (c-package (make-package (format nil "~A.PROGRAM"
                                          (string-upcase
                                           (file-namestring c-file)))
                                  :use ())))
    (let ((*package* c-package))
      (load-c-file c-file))
    (del "a.out")
    (ccl:save-application
     "a.out"
     :toplevel-function (lambda ()
                          (run-c-program c-package))
     :error-handler     :quit
     :prepend-kernel    t)))

(let ((vcc-out (merge-pathnames
                "vcc"
                #.(directory-namestring
                   (or *load-truename* *compile-file-truename*)))))
  (del vcc-out)
  (ccl:save-application
   vcc-out
   :toplevel-function #'main
   :error-handler     :quit
   :prepend-kernel    t))
