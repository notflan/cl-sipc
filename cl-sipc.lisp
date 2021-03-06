;; cl-sipc

(in-package :cl-sipc)
(use-package :cl-sipc-mem)

(defparameter *on-error* #'(lambda (err) nil))
(defparameter *on-message* #'(lambda (msg)
			       (not (eql :close (car msg)))))

;; wrappers

(defun timeout (sd value)
  "set timeout for this socket (0 is infinite)"
  (when value
    (si-timeout sd value))
  sd)

(defun bind (file &key (read-timeout nil))
  "bind to the AF_UNIX socket `file'
   returns the sd on success, nil on failure.
   timeout can set a read timeout for the connection. 0 (or nil) is forever.
   (remember it has to be deleted first!)
  "
  (let ((rc (si-bind file)))
    (if (< rc 0) ;error
      nil
      (timeout rc read-timeout))))

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
	(*on-error* on-err))
    (let ((rc (si-listen sd (callback si-error-callback) (callback si-callback))))
      (if (< rc 0)
	nil
	rc))))

(defun release (sd)
  "close a socket `sd'"
  (si-close sd)
  t)

(defun connect (file &key (read-timeout nil))
  "connect to socket `file'"
  (let ((rc (si-connect file)))
    (if (< rc 0) ;error
      nil
      (timeout rc read-timeout))))

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

(defun %siqs-binary-r (sd value flags resp)
  (cond ((pointer-p value) (siqs-binary-r sd (pointer-memory value) (pointer-size value) flags resp ))
	(t (with-pointer (ptr) value (%siqs-binary-r sd ptr flags resp  ))))) 

(defun %siqr-binary (sd value)
  (cond ((pointer-p value) (siqr-binary sd (pointer-memory value) (pointer-size value)))
	(t (with-pointer (ptr) value (%siqr-binary sd ptr)))))

(defun send (sd value &optional (type :string) (keep-resp t))
  "send to sever on socket sd. 
   example: (with-bound-socket (socket \"file.socket\") (hook socket ...))
   returns (values response(t if none or keep-resp is nil) nil) on success. (values nil <error>) on failure.
   error can be:
   	:partial - Could not write whole message
	:error - send() error
	:failure - send failed
	:message - invalid response message
	:checksum - invalid checksum
	:unknown - unknown error code
	:unknown-type - key argument :type is unknown
  :type can be:
  	:string (default) - assumes `value' is string, send that as string type
	:binary - assumes `value' is either string or vector of bytes, send that as binary type
	:close - ignore value, send close signal
  "
  (let* ((response (sif-heap-alloc (if keep-resp 1 0)))
	 (rc (cond ((eql type :string) (siqs-string-r sd value 0 response))
		  ((eql type :binary) (%siqs-binary-r sd value 0 response))
		  ((eql type :close) (siqs-close-r sd 0 response))
		  (t :unknown-type))))
    (if (numberp rc)
      (if (= rc #.+si-send-okay+)
	(values
	  (if (null-pointer-p response)
	    t
	    (let ((resp (mem-ref response :pointer)))
	      (prog1
		(if (null-pointer-p resp)
		  t
		  (let ((resp-val nil))
		    (si-typecase resp
			         :string #'(lambda (value) (setf resp-val value)
			         :binary #'(lambda (value) (setf resp-val value)
			         :close  #'(lambda (value) (setf resp-val value)))))
		    resp-val))
		(sif-heap-free response))))
	  nil)
	(values (progn
		  (sif-heap-free response)
		  nil)
		(cond ((= rc #.+si-send-partial+) :partial)
		      ((= rc #.+si-send-error+) :error)
		      ((= rc #.+si-send-failure+) :failure)
		      (t (marshal-ec rc)))))
      rc)))

(defun respond (value &optional (type :string))
  "send a response. (works same as send() but for server instead of client)
   additional return values can be:
   	:response-invalid - this message cannot be responded to
	:response-disabled - the client does not want a response
	:response-multi - a response has already been sent
  "
  (let ((sd (symbol-value '*response-message*)))
    (if sd
      (let ((rc (cond 
		  ((eql type :string) (siqr-string sd value))
		  ((eql type :binary) (%siqr-binary sd value))
		  ((eql type :close) (siqr-close sd))
		  (t :unknown-type))))
	(if (numberp rc)
	  (cond ((= rc #.+si-send-okay+) t)
		((= rc #.+sie-r-disable+) :response-disabled)
		((= rc #.+sie-r-multi+) :response-multi)
		((= rc #.+sie-r-invalid+) :response-invalid)
		((= rc #.+si-send-partial+) :partial)
		((= rc #.+si-send-error+) :error)
		((= rc #.+si-send-failure+) :failure)
		(t (marshal-ec rc)))
	  rc))
      :response-invalid)))

(defun send-quick (sock value &optional (type :string))
  "Quickly send value to socket file `sock'"
  (with-bound-socket (sd sock :connect)
    (send sd value type)))

(mapc #'export '(connect respond timeout send send-quick bind hook release with-bound-socket))
