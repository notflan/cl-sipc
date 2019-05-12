;; Simple sipc server

(let ((*standard-output* (make-broadcast-stream)))
  (ql:quickload :cl-sipc))

(defparameter *socket-file* "sipc.socket")
(defparameter *socket* (cl-sipc:bind *socket-file*))

(when (not *socket*)
  (format t "Error binding ~a~%" *socket-file*) (quit))

(let ((rc (cl-sipc:hook *socket*
			#'(lambda (err)
			    (format t "Error: ~a~%" err)
			    nil)
			#'(lambda (type message)
			    (format t "<- (~a) ~a~%" type message)
			    (not (eql :close type))))))
  (format t "Listen rc ~a~%" rc)
  (cl-sipc:release *socket*))
