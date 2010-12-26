
(defpackage :userial-tests
  (:use :cl :userial)
  (:import-from :userial :make-buffer
		         :make-displaced-buffer
		         :make-bitfield-serializer
		         :make-enum-serializer
		         :rewind-buffer
		         :serialize
		         :serialize*
		         :unserialize
		         :unserialize*
		         :unserialize-let*))
