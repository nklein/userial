
(defpackage :userial
  (:use :cl)
  (:export :buffer
           :make-buffer
	   :with-buffer
	   :buffer-length
	   :buffer-capacity
	   :buffer-advance
	   :buffer-add-byte
	   :buffer-get-byte
	   :buffer-rewind
	   :make-bitfield-serializer
	   :make-enum-serializer
	   :make-float-serializer
	   :make-int-serializer
	   :make-uint-serializer
	   :make-slot-serializer
	   :make-accessor-serializer
	   :serialize
	   :serialize*
	   :unserialize
	   :unserialize*
	   :unserialize-let*
	   :unserialize-list*))
