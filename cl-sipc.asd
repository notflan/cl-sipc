

(asdf:defsystem #:cl-sipc
  :description "AF_UNIX IPC for CL"
  :author "Avril <flanchan@cumallover.me>"
  :license "None"
  :version "0.0.4"
  :serial t
  :depends-on (  :cffi  )
  :components  ((:file "package")
		(:file "ffi")
		(:file "pointer")
		(:file "cl-sipc")))
