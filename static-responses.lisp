(in-package :bel-serve)

(defparameter *error-response*
  (create-response *error*
		   "<!DOCTYPE html>
<html>
<head>
<title>500 Internal Server Error</title>
</head>
<body>
<h1>Internal Server Error</h1>
<p>The server has encountered an internal error</p>
<hr>
</body>
</html>"))

(defparameter *not-found-response*
  (create-response *not-found*
		   "<!DOCTYPE html>
<html>
<head>
<title>404 Not Found</title>
</head>
<body>
<h1>Not Found</h1>
<p>The requested URL was not found on this server.</p>
<hr>
</body>
</html>"))

(defparameter *bad-request-response*
  (create-response *bad-request*
		   "<!DOCTYPE html>
<html>
<head>
<title>400 Bad Request</title>
</head>
<body>
<h1>Bad Request</h1>
<p>The request was not a properly formed HTTP request</p>
<hr>
</body>
</html>"))

(defparameter *forbidden-response*
  (create-response *forbidden*
		   "<!DOCTYPE html>
<html>
<head>
<title>403 Forbidden</title>
</head>
<body>
<h1>Forbidden</h1>
<p>You are forbidden from accessing the requested URL on this server.</p>
<hr>
</body>
</html>"))