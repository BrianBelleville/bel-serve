(in-package :bel-serve)

(defun make-base-header-fields ()
  (let ((ret (make-hash-table)))
    (setf (gethash :server ret) *server-rev*)
    ret))

(defun make-base-response ()
  (make-http-response
   :http-version *http-version*
   :header-fields (make-base-header-fields)))

(defun create-response (status-code-val &optional body-val)
  (let ((rval (make-base-response)))
    (with-slots (status-code reason-phrase body) rval
      (setf status-code status-code-val
	    reason-phrase (gethash status-code-val *status-codes* "no phrase")
	    body body-val)
      rval)))


(defun make-ok-response ()
  (create-response *ok*))

(defun append-path (app-root uri)
  (concatenate 'string (namestring app-root) (subseq uri 1)))