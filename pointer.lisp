;; pointer

(in-package :cl-sipc)

(defstruct pointer
  memory
  size)


(defun pointer-from-string (str)
  "String to pointer"
  (let ((ptr (foreign-string-alloc str)))
    (make-pointer :memory ptr :size (foreign-funcall "strlen" :pointer ptr :int))))

(defun pointer-from-seq (vec &optional (type :unsigned-char))
  "Vector or list to pointer"
  (let ((ptr (foreign-alloc type :initial-contents vec)))
    (make-pointer :memory ptr :size (* (foreign-type-size type) (length vec)))))

(defun pointer-from (value &optional (type :unsigned-char))
  "Value to new allocated pointer"
  (pointer-from-sequence (list value) type))

(defmacro with-pointer (desc from &body body)
  "with pointer allocated
   desc can have name, type allocated, C type.
   example: 
   	(with-pointer (string-ptr) lisp-string
		      body...)
	(with-pointer (string-ptr-explicit :string) lisp-string
		      body...)
	(with-pointer (vector :sequence :int) vector-of-ints
		      body...)
	(with-pointer (vector :infer :char) vector-of-chars
		      body...)
  "
  (let ((name (car desc))
	(type0 (or (cadr desc) :infer))
	(type (or (caddr desc) :unsigned-char))
	(pointer-from-type-infer (gensym)))
    (let ((makeptr
	    (cond ((eql type0 :string) `(pointer-from-string ,from))
	  	  ((eql type0 :sequence) `(pointer-from-seq ,from ,type))
	 	  ((eql type0 :single) `(pointer-from ,from ,type))
		  (t `(,pointer-from-type-infer ,from ,type)))))
      `(flet ((,pointer-from-type-infer (fr ty)
					(cond ((stringp fr) (pointer-from-string fr))
					      ((or (vectorp fr)
						   (listp fr)) (pointer-from-seq fr ty))
					      (t (pointer-from fr ty)))))
	 (let* ((,name ,makeptr)
		(result
		  (progn ,@body)))
	   (pointer-free ,name)
	   result)))))

(defun pointer-free (ptr)
  "Free pointer"
  (foreign-free (pointer-memory ptr)))

(mapc #'export '( make-pointer
		  pointer
		  pointer-memory
		  pointer-p
		  pointer-size

		  pointer-from-string
		  pointer-from-seq
		  pointer-from
		  with-pointer
		  pointer-free))
