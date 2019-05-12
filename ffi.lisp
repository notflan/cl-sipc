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

(defctype si-send-rc :int)
(defconstant +si-send-okay+ 0)
(defconstant +si-send-partial+ 1)
(defconstant +si-send-error+ -1)
(defconstant +si-send-failure+ -2)

;; library definition

(define-foreign-library libsipc
  (:unix (:or "libsipc.so" "libsipc-ffi.so" "./libsipc.so"))
  (t (:default "libsipc")))

(use-foreign-library libsipc)

;; FFI helper functions

(defcfun "sif_type" si-type (message :pointer))
(defcfun "sif_size" :unsigned-int (message :pointer))
(defcfun "sif_data" :pointer (message :pointer))

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
 
(defcallback si-callback :int ((message :pointer))
  (let* ((type (sif-type message))
	(size (sif-size message))
	(data (sif-data message))
	(rval (cond ((= type #.+si-string+) (funcall (symbol-value '*on-message*) :string (foreign-string-to-lisp data)))
		    ((= type #.+si-close+) (funcall (symbol-value '*on-message*) :close nil))
		    ((= type #.+si-binary+) (funcall (symbol-value '*on-message*) :binary data)))))
    (if rval
      0
      1)))

;; libsipc functions

(defcfun "si_bind" :int
  (file :string))

(defcfun "si_listen" :int
  (server :int)
  (on-error :pointer)
  (on-message :pointer))

(defcfun "si_close" :void
  (server :int))
