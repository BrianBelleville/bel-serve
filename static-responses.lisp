(in-package :bel-serve)


(defvar *error-response*
  (create-response *error*
		   "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">
<html><head>
<title>500 Internal Server Error</title>
</head><body>
<h1>Not Found</h1>
<p>The server has encountered an internal error</p>
<hr>"))

(defvar *not-found-response*
  (create-response *not-found*
		   "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">
<html><head>
<title>404 Not Found</title>
</head><body>
<h1>Not Found</h1>
<p>The requested URL was not found on this server.</p>
<hr>"))

(defvar *bad-request-response*
  (create-response *bad-request*
		   "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">
<html><head>
<title>400 Bad Request</title>
</head><body>
<h1>Bad Request</h1>
<p>The request was not a properly formed HTTP request</p>
<hr>"))

(defvar *forbidden-response*
  (create-response *forbidden*
		   "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">
<html><head>
<title>403 Forbidden</title>
</head><body>
<h1>Forbidden</h1>
<p>You are forbidden from accessing the requested URL on this server.</p>
<hr>"))