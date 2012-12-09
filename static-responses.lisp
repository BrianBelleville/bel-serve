(in-package :bel-serve)


(defvar *error-response*
  (create-response *error*
		   "server error"))

(defvar *not-found-response*
  (create-response *not-found*
		   "not found"))

(defvar *bad-request-response*
  (create-response *bad-request*
		   "bad request"))

(defvar *forbidden-response*
  (create-response *forbidden*
		   "forbidden"))