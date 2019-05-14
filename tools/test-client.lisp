

(let ((*standard-output* (make-broadcast-stream)))
  (ql:quickload :cl-sipc))

(defparameter *socket-file* "sipc.socket")

(defun help (&optional (st t))
  (format t "Available commands:~% (send hello-world)~% (send-binary hello-world)~% (close)~% (rebind \"socket-file.socket\")~% help~% quit~%")
  (when st
    (format t "(bound to \"~a\")~%" *socket-file*))
  'unknown-command)

(format t "Working with socket \"~a\"~%" *socket-file*)
(help nil)

(loop while t do
  (let ((rd (progn
	      (format t "> ")
	      (force-output)
	      (read))))
    (and (atom rd)
	 (eql rd 'quit)
	 (exit))
    (print (and (not (atom rd))
	 (cond ((eql (car rd) 'send) (sipc:send-quick *socket-file* (if (stringp (cadr rd))
								      (cadr rd)
								      (write-to-string (cadr rd))) :string))
	       ((eql (car rd) 'send-binary) (sipc:send-quick *socket-file* (if (stringp (cadr rd))
									    (cadr rd)
									    (write-to-string (cadr rd))) :binary))
	       ((eql (car rd) 'close) (sipc:send-quick *socket-file* nil :close))
	       ((eql (car rd) 'rebind) (setf *socket-file* (cadr rd)))
	       (t (help)))))
    (format t "~%")
    (and (atom rd)
	 (help))))
