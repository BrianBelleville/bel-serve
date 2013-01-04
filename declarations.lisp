(in-package :bel-serve)

(defparameter *server-rev* "bel-serve/0.0.2")

(defvar *mime-types*
  (assoc-list-to-hash-table
   '((:txt "text/plain")
     (:html "text/html")
     (:css "text/css")
     (:jpg "image/jpg")
     (:jpeg "image/jpeg")
     (:gif "image/gif")
     (:pdf "application/pdf"))))

(defparameter *http-version* "HTTP/1.1")

(defparameter *status-codes* (assoc-list-to-hash-table
			      '((200 "OK")
				(400 "Bad Request")
				(403 "Forbidden")
				(404 "Not Found")
				(500 "Internal Server Error"))))
				
(defparameter *bad-request* 400)
(defparameter *forbidden* 403)
(defparameter *not-found* 404)
(defparameter *ok* 200)
(defparameter *error* 500)

(defvar *root-dir*) ; the root directory of the page
(defvar *root-file-path*) ; the file that should be served for requests to the root directory

(defparameter *number-of-threads* 4)

(defvar *connection-queue* (make-instance 'queue))
(defvar *con-count-sem* (make-semaphore))
(defvar *con-queue-mutex* (make-mutex))
