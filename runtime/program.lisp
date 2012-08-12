(in-package #:vacietis)
(in-readtable vacietis)

(defvar *the-environment* (make-hash-table :test 'equal))

(defun run-c-program (program-package &key
                      (stdin  *standard-input*)
                      (stdout *standard-output*)
                      (stderr *error-output*))
  (flet ((make-c-stream (x) (make-instance 'vacietis.stdio::FILE :stream x)))
    (let ((vacietis.stdio::stdin  (make-c-stream stdin))
          (vacietis.stdio::stdout (make-c-stream stdout))
          (vacietis.stdio::stderr (make-c-stream stderr))
          (*the-environment*      (make-hash-table :test 'equal)))
      (catch 'c-exit
        (funcall (find-symbol "MAIN" program-package))))))
