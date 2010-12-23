
(in-package :unet)

(defstruct (unet (:constructor internal-make-unet))
  (socket nil :type usocket:datagram-usocket))

(declaim (ftype (function (&optional (integer 0 65536)) unet) make-unet))
(defun make-unet (&optional (port 26354))
  (declare (type (integer 0 65535) port))
  (internal-make-unet :socket (usocket:socket-connect nil nil
						      :protocol :datagram
						      :local-port port)))

(declaim (ftype (function (unet)) unet-close))
(defun unet-close (unet)
  (declare (type unet unet))
  (usocket:socket-close (unet-socket unet))
  (values))
