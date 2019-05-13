;; cl-sipc

(in-package :cl-sipc)

(defparameter *on-error* #'(lambda (err) nil))
(defparameter *on-message* #'(lambda (msg)
			       (not (eql :close (car msg)))))

;; wrappers

(defun bind (file)
  "bind to the AF_UNIX socket `file'
   returns the sd on success, nil on failure
   (remember it has to be deleted first!)
  "
  (let ((rc (si-bind file)))
    (if (< rc 0) ;error
      nil
      rc)))

(defun hook (sd on-err on-msg)
  "listen on socket `sd'
   on error call the callback `on-err' with args (error)
   on message call the callback `on-msg' with args (type message)
   type can be:
  	:string - a string
	:binary - (TODO) a foreign pointer
	:close - a close request
   returns rc on success (1), and nil on failure
   (note: this function blocks until the connection closes)
  "
  (let ((*on-message* on-msg)
	(*on-err* on-err))
    (let ((rc (si-listen sd (callback si-error-callback) (callback si-callback))))
      (if (< rc 0)
	nil
	rc))))

(defun release (sd)
  "close a socket `sd'"
  (si-close sd)
  t)

(defun connect (file)
  "connect to socket `file'"
  (let ((rc (si-connect file)))
    (if (< rc 0) ;error
      nil
      rc)))

(defmacro with-bound-socket (desc &body body)
  "bind socket, run `body', then close the socket.
   if :connect is in desc, connect instead of binding.
   example: (with-bound-socket (socket \"file.socket\") (hook socket ...))
   example: (with-bound-socket (socket \"file.socket\" :connect) (send socket ...))
   returns the last value from body on success, returns NIL without running body if the socket failed to bind
  "
  `(let* ((,(first desc) (if (member :connect ',(cddr desc))
			   (connect ,(second desc))
			   (bind ,(second desc))))
	  (return-value 
	    (if (null ,(first desc)) ;;bind failed
	      nil
	      (progn ,@body))))
     (when (not (null ,(first desc)))
       (release ,(first desc)))
     return-value))

(defun %siqs-binary (sd value)
  (cond ((pointer-p value) (siqs-binary sd (pointer-memory value) (pointer-size value)))
	(t (with-pointer (ptr) value (%siqs-binary sd ptr)))))

(defun send (sd value &optional (type :string))
  "send to sever on socket sd.
   example: (with-bound-socket (socket \"file.socket\") (hook socket ...))
   returns (values t nil) on success. (values nil <error>) on failure.
   error can be:
   	:partial - Could not write whole message
	:error - send() error
	:failure - send failed
	:unknown - unknown error code
	:unknown-type - key argument :type is unknown
  :type can be:
  	:string (default) - assumes `value' is string, send that as string type
	:binary - assumes `value' is either string or vector of bytes, send that as binary type
	:close - ignore value, send close signal
  "
  (let ((rc (cond ((eql type :string) (siqs-string sd value))
		  ((eql type :binary) (%siqs-binary sd value))
		  ((eql type :close) (siqs-close sd))
		  (t :unknown-type))))
    (if (numberp rc)
      (if (= rc #.+si-send-okay+)
	(values t nil)
	(values nil
		(cond ((= rc #.+si-send-partial+) :partial)
		      ((= rc #.+si-send-error+) :error)
		      ((= rc #.+si-send-failure+) :failure)
		      (t :unknown))))
      rc)))

(defun send-quick (sock value &optional (type :string))
  "Quickly send value to socket file `sock'"
  (with-bound-socket (sd sock :connect)
    (send sd value type)))

(mapc #'export '(connect send send-quick bind hook release with-bound-socket))
