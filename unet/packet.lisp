
(in-package :unet)

;;; packet type declaration
(deftype packet ()
  "A PACKET is a one-dimensional array of unsigned bytes with a fill-pointer"
  '(and (array (unsigned-byte 8) (*)) (not simple-array)))

;;; make-packet function
(declaim (ftype (function (&optional (integer 4 *)) packet) make-packet))
(defun make-packet (&optional (max-length 32768))
  "Create an empty PACKET of a given MAX-LENGTH"
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
  "Create a PACKET which refers to the same data as the given PACKET but with a different FILL-POINTER"
  (declare (type packet packet)
	   (type integer fill-pointer))
  (make-array (array-dimensions packet)
	      :element-type '(unsigned-byte 8)
	      :displaced-to packet
	      :fill-pointer fill-pointer))

;;; reset a packet
(declaim (ftype (function (packet) packet) rewind-packet))
(defun rewind-packet (packet)
  "Make a PACKET that refers to the given PACKET but with the FILL-POINTER reset to zero."
  (declare (type packet packet))
  (make-displaced-packet packet))
