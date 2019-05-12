;; Simple sipc server

(let ((*standard-output* (make-broadcast-stream)))
  (ql:quickload :cl-sipc))

(defparameter *socket-file* "sipc.socket")
(defparameter *socket* (cl-sipc:bind *socket-file*)) ;;attempt to bind to this file

(when (not *socket*) 
  (format t "[e] binding failed ~a~%" *socket-file*) (quit))

(format t "[+] listening on ~a...~%" *socket-file*)

;;block until the listener is done
(let ((rc (cl-sipc:hook *socket*
			#'(lambda (err) ;; Callback ran if there is an error
			    (format t "Error: ~a~%" err)
			    nil) ;;returning NIL to the listener stops
			#'(lambda (type message) ;; Callback ran when a message is received
			    (format t " <- (~a) ~a~%" type message) ;;print the message & type
			    (not (eql :close type)))))) ;;returning NIL if the type is :CLOSE to stop the listener
  (format t "[-] listen rc ~a~%" rc)
  (cl-sipc:release *socket*)) ;;finally, release the socket

(quit)
