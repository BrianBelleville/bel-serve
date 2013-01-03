(declaim (optimize (speed 3)))

(defpackage :bel-serve
  (:use :cl
	:sb-bsd-sockets
	:sb-thread
	:bel-http
	:bel-utils
	:first-gen-server-hooks))