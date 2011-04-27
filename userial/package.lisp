;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(defpackage :userial
  (:use :cl)
  (:export :buffer
           :*buffer*
           :make-buffer
	   :with-buffer
	   :buffer-length
	   :buffer-capacity
	   :buffer-advance
	   :buffer-add-byte
	   :buffer-get-byte
	   :buffer-rewind
	   :make-uint-serializer
	   :make-int-serializer
	   :make-enum-serializer
	   :make-bitfield-serializer
	   :make-float-serializer
           :make-simple-serializer
	   :make-slot-serializer
	   :make-accessor-serializer
	   :serialize
	   :serialize*
	   :serialize-slots*
	   :serialize-accessors*
	   :unserialize
	   :unserialize*
	   :unserialize-let*
	   :unserialize-list*
	   :unserialize-accessors*
	   :unserialize-slots*
	   :serialize-log
	   :peek
	   :with-peek-buffer))
