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

;; todo : send-resource and send-string should probably be the same generic function with dispatch on type, so that the sending of dynamically generated content and static content uses the same function. This could also allow for handlers to return different types of data than a string
(defun send-resource (stream  pathname)
  (declare (type stream stream))
  (let ((response (make-ok-response)))
    (set-content-type response pathname)
    (setf (http-response-body response) (open pathname :element-type '(unsigned-byte 8) )) 
    (send-response stream response)))

(defun send-string (stream string)
  (declare (type stream stream)
	   (type string string))
  (let ((response (make-ok-response)))
    (setf (gethash :content-type
		   (http-response-header-fields response))
	  "text/html")
    (setf (http-response-body response) string)
    (send-response stream response)))

(defun service-get (request stream)
  (declare (type http-request request)
	   (type stream stream))
  (multiple-value-bind (gen-html found) (execute-get-method (http-request-request-uri request))
    (if found
	(send-string stream gen-html)
	(aif (find-resource request)
			(send-resource stream it)
			(send-response stream *not-found-response*)))))

(defun service-request (remote-socket)
  (multiple-value-bind (request stream) (receive-request remote-socket)
    (declare (type stream stream)) ; request may be null if there is an error with what was read from the stream
    (handler-case
	(if request
	    ;right now only suport GET requests, in the future maybe split out this cond clause to handle the variety of request types.
	    (cond ((equal (http-request-method request) "GET")
		   (service-get request stream))
		  (t (send-response stream *forbidden-response*)))
	    (send-response stream *bad-request-response*))
      (socket-error () ) ; socket error, do nothing, socket will be closed after the form is exited
      (error () (send-response stream *error-response*)))
    (close stream)))

(defun accept-connections-loop (listen-socket)
  (declare (type inet-socket listen-socket))
  (unwind-protect
       (loop
	    (let ((remote-socket (socket-accept listen-socket)))
	      (with-mutex (*con-queue-mutex*)
		(enqueue *connection-queue* remote-socket))
	      (signal-semaphore *con-count-sem*)))
    (socket-close listen-socket)))

(defun service-connections-loop ()
  (loop
     (wait-on-semaphore *con-count-sem*)
     (let ((remote-socket (with-mutex (*con-queue-mutex*)
			    (dequeue *connection-queue*))))
       (service-request remote-socket))))
     
(defun start-server (&optional (port 8080) (root-dir *root-dir*) (root-file-path *root-file-path*))
  "starts an http server listening at port. It will serve files from root-dir, and requests for root will be answered with the file at root-file-path"
  (setf *root-dir* root-dir
	*root-file-path* root-file-path)
  (let ((listen-socket (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (progn
      (socket-bind listen-socket '(0 0 0 0) port)
      (socket-listen listen-socket 256)
      (make-thread (lambda () (accept-connections-loop listen-socket))
		   :name "Accept Connections Thread")
      (loop
	 for i from 1 to *number-of-threads* do
	   (make-thread #'service-connections-loop
			:name (format nil "Service Connections Thread ~D" i)))))
  t) ;return t