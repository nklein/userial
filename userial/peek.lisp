;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :userial)

(defmacro with-peek-buffer ((var &key (buffer 'userial::*buffer*)) &body body)
  (let ((buf (gensym "BUFFER-")))
    `(let* ((,buf ,buffer)
	    (,var (make-array (list (userial:buffer-capacity :buffer ,buf))
			      :element-type '(unsigned-byte 8)
			      :displaced-to ,buf
			      :fill-pointer (userial:buffer-length
					                  :buffer ,buf)
			      :adjustable nil)))
       (with-buffer ,var
	 ,@body))))

(defun peek (type &rest rest &key (buffer *buffer*) &allow-other-keys)
  (remf rest :buffer)
  (with-peek-buffer (peekbuf :buffer buffer)
    (apply #'unserialize type :buffer peekbuf rest)))
