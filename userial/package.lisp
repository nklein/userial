
(defpackage :userial
  (:use :cl)
  (:export :make-buffer
	   :make-displaced-buffer
	   :make-bitfield-serializer
	   :make-enum-serializer
	   :make-float-serializer
	   :make-int-serializer
	   :make-uint-serializer
	   :add-bytes-from-uint
	   :get-bytes-as-uint
	   :rewind-buffer
	   :serialize
	   :serialize*
	   :unserialize
	   :unserialize*
	   :unserialize-let*))
