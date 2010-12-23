
(in-package :unet-tests)

(nst:def-criterion-alias (:array-equalp bytes)
    `(:equalp ,(make-array (list (length bytes))
			   :element-type '(unsigned-byte 8)
			   :initial-contents bytes)))

(nst:def-test-group test-serialize* ()
  (nst:def-test serialize-uint8 (:array-equalp (0 255 1 128))
    (serialize* (:uint8 0 :uint8 255 :uint8 1 :uint8 128) (make-packet 4)))
  (nst:def-test serialize-int8 (:array-equalp (128 129 127 255 0))
    (serialize* (:int8 0 :int8 1 :int8 -1 :int8 127 :int8 -128)
		(make-packet 5)))
  (nst:def-test serialize-uint16 (:array-equalp (0 0  1 0  4 163  255 255))
    (serialize* (:uint16 0 :uint16 256 :uint16 1187 :uint16 65535)
		(make-packet 8)))
  (nst:def-test serialize-int16 (:array-equalp (128   0    128   1     127 255
                                                129   0    127   0
						255 255      0   0))
    (serialize* (:int16 0 :int16 1 :int16 -1
		 :int16 256 :int16 -256
		 :int16 32767 :int16 -32768) (make-packet 14))))
