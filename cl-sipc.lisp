;; cl-sipc

(in-package :cl-sipc)

(defparameter *on-error* #'(lambda (err) nil))
(defparameter *on-message* #'(lambda (msg)
			       (not (eql :close (car msg)))))

;; wrappers

(defun bind (file)
  "bind to the AF_UNIX socket `file'"
  "returns the sd on success, nil on failure"
  "(remember it has to be deleted first!)"
  (let ((rc (si-bind file)))
    (if (< rc 0) ;error
      nil
      rc)))

(defun hook (sd on-err on-msg)
  "listen on socket `sd'"
  "on error call the callback `on-err' with args (error)"
  "on message call the callback `on-msg' with args (type message)"
  "type can be:
  	:string - a string
	:binary - (TODO) a foreign pointer
	:close - a close request"
  "returns rc on success (1), and nil on failure"
  "(note: this function blocks until the connection closes)"
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

(mapc #'export '(bind hook release))
