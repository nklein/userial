
(in-package :unet)

;;; packet type declaration
(deftype packet () '(and (array (unsigned-byte 8) (*)) (not simple-array)))

;;; make-packet function
(declaim (ftype (function (&optional (integer 4 *)) packet) make-packet))
(defun make-packet (&optional (max-length 32768))
  (declare (type (integer 4 *) max-length))
  (let ((packet (make-array (list max-length)
			    :element-type '(unsigned-byte 8)
			    :initial-element 0
			    :fill-pointer 0)))
    (declare (type packet packet))
    packet))

;;; make-displaced-packet function
(declaim (ftype (function (packet &optional integer) packet)
		make-displaced-packet))
(defun make-displaced-packet (packet &optional (fill-pointer 0))
  (declare (type packet packet)
	   (type integer fill-pointer))
  (make-array (array-dimensions packet)
	      :element-type '(unsigned-byte 8)
	      :displaced-to packet
	      :fill-pointer fill-pointer))

;;; reset a packet
(declaim (ftype (function (packet) packet) rewind-packet))
(defun rewind-packet (packet)
  (declare (type packet packet))
  (make-displaced-packet packet))
