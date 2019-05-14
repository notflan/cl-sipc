;; Foreign functions

(in-package :cl-sipc)

(defctype si-type :int)
(defconstant +si-string+ 0)
(defconstant +si-binary+ 1)
(defconstant +si-close+ 2)

(defctype si-error :int)
(defconstant +sie-accept+ 0)
(defconstant +sie-read+ 1)
(defconstant +sie-pconcls+ 2)
(defconstant +sie-invalid+ 3)
(defconstant +sie-r-invalid+ 4)
(defconstant +sie-r-multi+ 5)
(defconstant +sie-r-disable+ 6)

(defctype si-send-rc :int)
(defconstant +si-send-okay+ 0)
(defconstant +si-send-partial+ 1)
(defconstant +si-send-error+ -1)
(defconstant +si-send-failure+ -2)

;; params

(defparameter *response-message* nil)

;; library definition

(define-foreign-library libsipc
  (:unix (:or "libsipc.so" "libsipc-ffi.so" "./libsipc.so"))
  (t (:default "libsipc")))

(use-foreign-library libsipc)

;; FFI helper functions

(defcfun "sif_type" si-type (message :pointer))
(defcfun "sif_size" :unsigned-int (message :pointer))
(defcfun "sif_data" :pointer (message :pointer))
(defcfun "sif_heap_alloc" :pointer (keep-resp :int))
(defcfun "sif_heap_free" :void (resp :pointer))

(defcfun "si_error_string" :pointer (err si-error))

;; Callbacks

(defun marshal-ec (err)
  (cond ((= err #.+sie-accept+) :accept)
	((= err #.+sie-read+) :read)
	((= err #.+sie-pconcls+) :closed)
	((= err #.+sie-invalid+) :message)
	(t :unknown)))

(defcallback si-error-callback :int ((err si-error))
  (when (symbol-value '*on-error*)
    (if (funcall (symbol-value '*on-error*) (marshal-ec err))
      0
      1)))

(defun si-typecase (message &key string binary close)
  (let* ((type (sif-type message))
	(size (sif-size message))
	(data (sif-data message))
	(rval (cond ((= type #.+si-string+) (funcall string (foreign-string-to-lisp data)))
		    ((= type #.+si-close+) (funcall close nil))
		    ((= type #.+si-binary+) (funcall binary (make-pointer :memory data :size size))))))
    (if rval
      0
      1)))


(defcallback si-callback :int ((message :pointer))
  (let ((*response-message* message))
    (si-typecase message
	         :string #'(lambda (value) (funcall (symbol-value '*on-message*) :string value))
	         :binary #'(lambda (value) (funcall (symbol-value '*on-message*) :binary value))
	         :close  #'(lambda (avlue) (funcall (symbol-value '*on-message*) :close nil)))))
#|  "(let* ((type (sif-type message))
	(size (sif-size message))
	(data (sif-data message))
	(rval (cond ((= type #.+si-string+) (funcall (symbol-value '*on-message*) :string (foreign-string-to-lisp data)))
		    ((= type #.+si-close+) (funcall (symbol-value '*on-message*) :close nil))
		    ((= type #.+si-binary+) (funcall (symbol-value '*on-message*) :binary (make-pointer :memory data :size size))))))
    (if rval
      0
      1))))"|#

;; libsipc functions

(defcfun "si_bind" :int
  (file :string))

(defcfun "si_listen" :int
  (server :int)
  (on-error :pointer)
  (on-message :pointer))

(defcfun "si_close" :void
  (server :int))

(defcfun "si_connect" :int
  (file :string))

;; send functions

(defcfun "siqs_string" :int
  (sd :int)
  (string :string))

(defcfun "siqs_string_r" :int
  (sd :int)
  (string :string)
  (flags :unsigned-int)
  (resp :pointer))

(defcfun "siqs_close" :int
  (sd :int))

(defcfun "siqs_close_r" :int
  (sd :int)
  (flags :unsigned-int)
  (resp :pointer))

(defcfun "siqs_binary" :int
  (sd :int)
  (buffer :pointer)
  (size :int))

(defcfun "siqs_binary_r" :int
  (sd :int)
  (buffer :pointer)
  (size :int)
  (flags :unsigned-int)
  (resp :pointer))

(defcfun "siqr_string" :int
  (sd :pointer)
  (str :string))

(defcfun "siqr_binary" :int
  (sd :pointer)
  (str :pointer)
  (size :int))

(defcfun "siqr_close" :int
  (sd :pointer))


