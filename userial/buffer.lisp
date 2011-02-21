
(in-package :userial)

(deftype uchar () '(unsigned-byte 8))
(deftype uint  () '(integer 0 *))
(deftype buffer ()
  "A BUFFER is an adjustable, one-dimensional array of unsigned bytes with
   a fill-pointer."
  '(array uchar (*)))

(defconstant +default-buffer-capacity+ 32768)
(defconstant +default-buffer-expand+ 8192)

;;; make-buffer function
(declaim (ftype (function (&optional (integer 1 *)) buffer) make-buffer))
(defun make-buffer (&optional (initial-capacity +default-buffer-capacity+))
  "Create an empty BUFFER of a given INITIAL-CAPACITY"
  (declare (type uint initial-capacity))
  (make-array (list initial-capacity)
	      :element-type 'uchar
	      :initial-element 0
	      :fill-pointer 0
	      :adjustable t))

(declaim (special *buffer*)
	 (type buffer buffer))
(defvar *buffer* (make-buffer))

(declaim (inline buffer-length)
	 (ftype (function (&optional buffer) uint) buffer-length))
(defun buffer-length (&optional (buffer *buffer*))
  "Returns the current length of the BUFFER"
  (declare (type buffer buffer))
  (fill-pointer buffer))

(declaim (inline buffer-capacity)
	 (ftype (function (&optional buffer) uint) buffer-capacity))
(defun buffer-capacity (&optional (buffer *buffer*))
  "Returns the current capacity of the BUFFER"
  (declare (type buffer buffer))
  (array-dimension buffer 0))

(declaim (ftype (function (uint &optional buffer) buffer)
		(setf buffer-capacity)))
(defun (setf buffer-capacity) (new-capacity &optional (buffer *buffer*))
  "Sets the capacity of the BUFFER to NEW-CAPACITY"
  (declare (type buffer buffer)
	   (type uint new-capacity))
  (setf *buffer* (adjust-array buffer (list new-capacity)
			       :fill-pointer
			       (min (fill-pointer buffer) new-capacity))))

(declaim (ftype (function (&optional uint buffer) buffer)
		buffer-expand-if-needed))
(defun buffer-expand-if-needed (&optional (expand-by 1) (buffer *buffer*))
  "Expand the BUFFER if needed to accomodate EXPAND-BY more bytes"
  (let ((capacity    (buffer-capacity buffer))
	(needs-to-be (+ (buffer-length buffer) expand-by)))
    (when (< capacity needs-to-be)
      (setf (buffer-capacity buffer)
	    (max needs-to-be (min (* capacity 2)
				  (+ capacity +default-buffer-expand+))))))
  (setf *buffer* buffer))

(declaim (inline buffer-advance)
	 (ftype (function (&optional uint buffer) buffer) buffer-advance))
(defun buffer-advance (&optional (amount 1) (buffer *buffer*))
  "Advances the BUFFER by AMOUNT bytes"
  (declare (type uint amount)
	   (type buffer buffer))
  (buffer-expand-if-needed (+ (buffer-length buffer) amount 1) buffer)
  (incf (fill-pointer buffer) amount)
  buffer)

(declaim (ftype (function (uchar &optional buffer) buffer) buffer-add-byte))
(defun buffer-add-byte (byte &optional (buffer *buffer*))
  "Add the given BYTE to the BUFFER"
  (declare (type uchar byte)
	   (type buffer buffer))
  (let ((buffer (buffer-expand-if-needed 1 buffer)))
    (setf (aref buffer (buffer-length buffer)) byte)
    (buffer-advance 1 buffer)))

(declaim (ftype (function (&optional buffer) (values uchar buffer))
		buffer-get-byte))
(defun buffer-get-byte (&optional (buffer *buffer*))
  "Get a byte from the BUFFER"
  (values (aref buffer (buffer-length buffer))
	  (buffer-advance 1 buffer)))

(declaim (ftype (function (&optional buffer) buffer) buffer-rewind))
(defun buffer-rewind (&optional (buffer *buffer*))
  "Rewind the BUFFER to the beginning"
  (declare (type buffer buffer))
  (setf (fill-pointer buffer) 0)
  buffer)

(defmacro unroll-add-bytes (buffer-form value-form bytes)
  "Added BYTES bytes from the VALUE-FORM to the BUFFER-FORM (most-significant
   byte first)"
  (let ((value-sym (gensym "VALUE-"))
	(buffer-sym (gensym "BUFFER-")))
    `(let ((,buffer-sym ,buffer-form)
	   (,value-sym (ldb (byte ,(* bytes 8) 0) ,value-form)))
       (declare (type buffer ,buffer-sym)
		(type uint ,value-sym))
       (let* ,(loop :for ii :from (1- bytes) :downto 0
		    :collecting `(,buffer-sym (buffer-add-byte
					         (ldb (byte 8 ,(* 8 ii))
						      ,value-sym)
						 ,buffer-sym)))
	 ,buffer-sym))))

(defmacro unroll-get-bytes (buffer-form bytes)
  "Get BYTES bytes from the BUFFER-FORM (most-significant byte first)"
  (let ((buffer-sym (gensym "BUFFER-"))
	(value-sym (gensym "VALUE-")))
    `(let ((,buffer-sym ,buffer-form)
	   (,value-sym 0))
       (declare (type buffer ,buffer-sym)
		(type uint ,value-sym))
       (progn
	 ,@(loop :for ii :from (1- bytes) :downto 0
		 :collecting `(setf (ldb (byte 8 ,(* 8 ii)) ,value-sym)
				    (buffer-get-byte ,buffer-sym)))
	 (values ,value-sym ,buffer-sym)))))
