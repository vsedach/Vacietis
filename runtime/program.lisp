(in-package #:vacietis)
(in-readtable vacietis)

(defvar *environment* (make-hash-table :test 'equal))

(defun run-c-program (program-package &key
                      (stdin  *standard-input*)
                      (stdout *standard-output*)
                      (stderr *error-output*))
  (flet ((make-c-stream (x) (make-instance 'vacietis.libc.stdio.h::FILE :stream x)))
    (let ((vacietis.libc.stdio.h:stdin  (make-c-stream stdin))
          (vacietis.libc.stdio.h:stdout (make-c-stream stdout))
          (vacietis.libc.stdio.h:stderr (make-c-stream stderr))
          (*environment*                (make-hash-table :test 'equal)))
      (catch 'c-exit
        (funcall (find-symbol "MAIN" program-package))))))
