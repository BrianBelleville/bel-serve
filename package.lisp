(declaim (optimize (speed 3)))

(defpackage :bel-serve
  (:use :cl
	:sb-bsd-sockets
	:bel-http
	:bel-utils))