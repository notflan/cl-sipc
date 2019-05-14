;; Simple sipc server

(let ((*standard-output* (make-broadcast-stream)))
  (ql:quickload :cl-sipc))

(defparameter *socket-file* "sipc.socket")
(defparameter *respond* t)

(when (probe-file *socket-file*)
  (delete-file *socket-file*))

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
			    (when *respond*
			      (format t
				      " -> ~a~%"
				      (sipc:respond 
					(if (eql type :binary)
					  (format nil "~a" (sipc:pointer-to-array message))
					  (format nil "~a" message)))))
			    (if (eql type :binary)
			      (format t " <- (~a) ~a (size: ~a)~%" type (sipc:pointer-to-array message) (sipc:pointer-size message)) ;;print the binary message as an array of bytes, the type, & the size
			      (format t " <- (~a) ~a~%" type message)) ;;print the message & type
			    (not (eql :close type)))))) ;;returning NIL if the type is :CLOSE to stop the listener
  (format t "[-] listen rc ~a~%" rc)
  (cl-sipc:release *socket*)) ;;finally, release the socket

(quit)
