
(defpackage :unet-tests
  (:use :cl :unet)
  (:import-from :unet "MAKE-PACKET"
		      "MAKE-DISPLACED-PACKET"
		      "REWIND-PACKET"))
