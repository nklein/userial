
(defpackage :userial
  (:use :cl)
  (:export *buffer*
           :make-buffer
	   :with-buffer
	   :buffer-length
	   :buffer-capacity
	   :buffer-advance
	   :buffer-add-byte
	   :buffer-get-byte
	   :buffer-rewind
	   :unroll-add-bytes
	   :unroll-get-bytes
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
