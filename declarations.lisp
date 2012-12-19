(in-package :bel-serve)

(defvar *mime-types*
  (assoc-list-to-hash-table
   '((:txt "text/plain")
     (:html "text/html")
     (:css "text/css")
     (:jpg "image/jpg")
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

(export '(*root-dir* *root-file-path*))
(defvar *root-dir*) ; the root directory of the page
(defvar *root-file-path*) ; the file that should be served for requests to the root directory

(defvar *listen-socket*)
