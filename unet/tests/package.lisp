
(defpackage :unet-tests
  (:use :cl :unet)
  (:import-from :unet :make-packet
		      :make-displaced-packet
		      :make-bitfield-serializer
		      :make-enum-serializer
		      :rewind-packet
		      :serialize
		      :serialize*
		      :unserialize
		      :unserialize*
		      :unserialize-let*))
