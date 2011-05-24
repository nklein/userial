;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :userial)

(defmacro with-peek-buffer (() &body body)
  `(with-buffer (make-array (list (buffer-capacity))
                            :element-type '(unsigned-byte 8)
                            :displaced-to *buffer*
                            :fill-pointer (buffer-length)
                            :adjustable nil)
     ,@body))

(defun peek (type &rest rest &key &allow-other-keys)
  (with-peek-buffer ()
    (apply #'unserialize type rest)))
