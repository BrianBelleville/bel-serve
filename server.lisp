(in-package :bel-serve)

;;; An http server

(defun find-resource (request)
  (declare (type http-request request))
  (let ((uri (http-request-request-uri request)))
    (if (equal uri "/")
	(probe-file *root-file-path*)
	(let ((file (probe-file (append-path (probe-file *root-dir*) uri))))
	  (if (and file (pathname-name file)) ; if the file is found, and the result of pathname-name is not nil, it is assumed to be a regular file and not a directory
	      file)))))

(defun set-content-type (response resource-pathname)
  (let ((type (string-keyword (pathname-type resource-pathname)))
	(headers (http-response-header-fields response)))
    (multiple-value-bind (val found) (gethash type *mime-types*)
      (if found
	  (setf (gethash :content-type headers) val)
	  (error "unsupported content type")))))

(defun send-resource (connection  pathname)
  (let ((response (make-ok-response)))
    (set-content-type response pathname)
    (setf (http-response-body response) (open pathname :element-type '(unsigned-byte 8) )) 
    (send-response connection response)))

(defun service-request ()
  (multiple-value-bind (request socket) (receive-request *listen-socket*)
    (handler-case
	(if request
	    ;right now only suport GET requests, in the future maybe split out this cond clause to handle the variety of request types.
	    (cond ((equal (http-request-method request) "GET")
		   (aif (find-resource request)
			(send-resource socket it)
			(send-response socket *not-found-response*)))
		  (t (send-response socket *forbidden-response*)))
	    (send-response socket *bad-request-response*))
      (socket-error () ) ; socket error, do nothing, socket will be closed after the form is exited
      (error () (send-response socket *error-response*)))
    (socket-close socket)))

(export 'start-server)
(defun start-server (&optional (port 8080) (root-dir *root-dir*) (root-file-path *root-file-path*))
  "starts an http server listening at port. It will server files from root-dir, and requests for root will be answered with the file at root-file-path"
  (let ((*listen-socket* (make-instance 'inet-socket :type :stream :protocol :tcp))
	(*root-dir* root-dir)
	(*root-file-path* root-file-path))
    (unwind-protect
	 (progn
	   (socket-bind *listen-socket* '(0 0 0 0) port)
	   (socket-listen *listen-socket* 16)
	   (loop
	      (service-request)))
      (when *listen-socket* (socket-close *listen-socket*)))))