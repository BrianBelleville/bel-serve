(asdf:defsystem "bel-serve"
  :description "A basic http server built on top of bel-http"
  :author "Brian Belleville"
  :depends-on (:bel-utils :bel-http)
  :serial t
  :components ((:file "package")
	       (:file "declarations")
	       (:file "common-funcs")
	       (:file "static-responses")
	       (:file "server")))
	       