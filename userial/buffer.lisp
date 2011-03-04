
(in-package :userial)

;;;(declaim (optimize (speed 3)))

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

(defmacro with-buffer (buffer &body body)
  "Macro to do a BODY of statements with the given BUFFER as default."
  `(let ((userial::*buffer* ,buffer))
     ,@body))

(declaim (inline buffer-length)
	 (ftype (function (&key (:buffer buffer)) uint) buffer-length))
(defun buffer-length (&key (buffer *buffer*))
  "Returns the current length of the BUFFER"
  (declare (type buffer buffer))
  (fill-pointer buffer))

(declaim (ftype (function (uint &key (:buffer buffer)) buffer)
		(setf buffer-length)))
(defun (setf buffer-length) (new-length &key (buffer *buffer*))
  "Sets the length of the BUFFER to NEW-LENGTH"
  (declare (type buffer buffer)
	   (type uint new-length))
  (setf (fill-pointer buffer) new-length)
  buffer)

(declaim (inline buffer-capacity)
	 (ftype (function (&key (:buffer buffer)) uint) buffer-capacity))
(defun buffer-capacity (&key (buffer *buffer*))
  "Returns the current capacity of the BUFFER"
  (declare (type buffer buffer))
  (array-dimension buffer 0))

(declaim (ftype (function (uint &key (:buffer buffer)) buffer)
		(setf buffer-capacity)))
(defun (setf buffer-capacity) (new-capacity &key (buffer *buffer*))
  "Sets the capacity of the BUFFER to NEW-CAPACITY"
  (declare (type buffer buffer)
	   (type uint new-capacity))
  (adjust-array buffer (list new-capacity)
		:fill-pointer
		(min (buffer-length :buffer buffer) new-capacity)))

(declaim (ftype (function (&key (:amount uint) (:buffer buffer)) buffer)
		buffer-expand-if-needed))
(defun buffer-expand-if-needed (&key (amount 1) (buffer *buffer*))
  "Expand the BUFFER if needed to accomodate EXPAND-BY more bytes"
  (declare (type uint amount)
	   (type buffer buffer))
  (let ((capacity    (buffer-capacity :buffer buffer))
	(needs-to-be (+ (buffer-length :buffer buffer) amount)))
    (when (< capacity needs-to-be)
      (setf (buffer-capacity :buffer buffer)
	    (max needs-to-be (min (* capacity 2)
				  (+ capacity +default-buffer-expand+))))))
  buffer)

(declaim (inline buffer-advance)
	 (ftype (function (&key (:amount uint) (:buffer buffer)) buffer)
		buffer-advance))
(defun buffer-advance (&key (amount 1) (buffer *buffer*))
  "Advances the BUFFER by AMOUNT bytes"
  (declare (type uint amount)
	   (type buffer buffer))
  (buffer-expand-if-needed :amount (+ (buffer-length :buffer buffer)
				      amount)
			   :buffer buffer)
  (incf (fill-pointer buffer) amount)
  buffer)

(declaim (ftype (function (uchar &key (:buffer buffer)) buffer)
		buffer-add-byte))
(defun buffer-add-byte (byte &key (buffer *buffer*))
  "Add the given BYTE to the BUFFER"
  (declare (type uchar byte)
	   (type buffer buffer))
  (let ((buffer (buffer-expand-if-needed :amount 1 :buffer buffer)))
    (setf (aref buffer (buffer-length :buffer buffer)) byte)
    (buffer-advance :amount 1 :buffer buffer)))

(declaim (ftype (function (&key (:buffer buffer)) (values uchar buffer))
		buffer-get-byte))
(defun buffer-get-byte (&key (buffer *buffer*))
  "Get a byte from the BUFFER"
  (values (aref buffer (buffer-length :buffer buffer))
	  (buffer-advance :amount 1 :buffer buffer)))

(declaim (ftype (function (&key (:buffer buffer)) buffer) buffer-rewind))
(defun buffer-rewind (&key (buffer *buffer*))
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
						 :buffer ,buffer-sym)))
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
				    (buffer-get-byte :buffer ,buffer-sym)))
	 (values ,value-sym ,buffer-sym)))))
