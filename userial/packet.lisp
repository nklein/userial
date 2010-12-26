
(in-package :userial)

;;; buffer type declaration
(deftype buffer ()
  "A BUFFER is a one-dimensional array of unsigned bytes with a fill-pointer"
  '(and (array (unsigned-byte 8) (*)) (not simple-array)))

;;; make-buffer function
(declaim (ftype (function (&optional (integer 4 *)) buffer) make-buffer))
(defun make-buffer (&optional (max-length 32768))
  "Create an empty BUFFER of a given MAX-LENGTH"
  (declare (type (integer 4 *) max-length))
  (let ((buffer (make-array (list max-length)
			    :element-type '(unsigned-byte 8)
			    :initial-element 0
			    :fill-pointer 0)))
    (declare (type buffer buffer))
    buffer))

;;; make-displaced-buffer function
(declaim (ftype (function (buffer &optional integer) buffer)
		make-displaced-buffer))
(defun make-displaced-buffer (buffer &optional (fill-pointer 0))
  "Create a BUFFER which refers to the same data as the given BUFFER but with a different FILL-POINTER"
  (declare (type buffer buffer)
	   (type integer fill-pointer))
  (make-array (array-dimensions buffer)
	      :element-type '(unsigned-byte 8)
	      :displaced-to buffer
	      :fill-pointer fill-pointer))

;;; reset a buffer
(declaim (ftype (function (buffer) buffer) rewind-buffer))
(defun rewind-buffer (buffer)
  "Make a BUFFER that refers to the given BUFFER but with the FILL-POINTER reset to zero."
  (declare (type buffer buffer))
  (make-displaced-buffer buffer))
