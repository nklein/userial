
(in-package :userial-tests)

(defun serialize-unserialize (tag value &optional (buffer-size 64))
  (nth-value 0 (unserialize (rewind-buffer (serialize (make-buffer buffer-size)
						      tag
						      value))
			    tag)))

(nst:def-arbitrary-instance-type (bounded-integer :scalar t
						  :key ((low 0) (high 0)))
  (assert (< low high))
  (+ (rem (abs (nst:arbitrary 'integer)) (max (- high low) 1)) low))

(nst:def-criterion-alias (:sample-ints tag low high)
    `(:sample :sample-size 32
	      :domains ((x (bounded-integer :low ,low :high ,high)))
	      :verify (= x (serialize-unserialize ,tag x))))

(nst:def-test-group test-uint-serializing ()
  (nst:def-test serialize-uint8  (:sample-ints :uint8  0 255))
  (nst:def-test serialize-uint16 (:sample-ints :uint16 0 65535))
  (nst:def-test serialize-uint24 (:sample-ints :uint24 0 16777215))
  (nst:def-test serialize-uint32 (:sample-ints :uint32 0 4294967295))
  (nst:def-test serialize-uint48 (:sample-ints :uint48 0 281474976710655))
  (nst:def-test serialize-uint64 (:sample-ints :uint64 0 18446744073709551615))
)

(nst:def-test-group test-int-serializing ()
  (nst:def-test serialize-int8  (:sample-ints :int8  -128 127))
  (nst:def-test serialize-int16 (:sample-ints :int16 -32768 32767))
  (nst:def-test serialize-int32 (:sample-ints :int32 -2147483648 2147483647))
  (nst:def-test serialize-int64 (:sample-ints :int64 -9223372036854775808
					              9223372036854775807)))

(nst:def-criterion-alias (:array-equalp bytes)
    `(:equalp ,(make-array (list (length bytes))
			   :element-type '(unsigned-byte 8)
			   :initial-contents bytes)))

(make-enum-serializer     :alphas (:a :b :c :d :e :f :g :h :i :j :k :l :m
				   :n :o :p :q :r :s :t :u :v :w :x :y :z))
(make-bitfield-serializer :colors (:red :orange :yellow :green :blue
				   :indigo :violet :white :black))

(nst:def-test-group test-enum/bitfield-serializing ()
  (nst:def-test serialize-alpha-enum (:array-equalp (1 13 26))
    (serialize (serialize (serialize (make-buffer 4) :alphas :a)
			  :alphas :m)
	       :alphas :z))
  (nst:def-test serialize-colors-bitfield (:array-equalp (0 1  1 2  1 255))
    (serialize (serialize (serialize (make-buffer 6) :colors :red)
			  :colors '(:orange :black))
	       :colors '(:red :orange :yellow :green :blue :indigo
			 :violet :white :black))))

(nst:def-test-group test-serialize* ()
  (nst:def-test serialize-uint8 (:array-equalp (0 255 1 128))
    (serialize* (:uint8 0 :uint8 255 :uint8 1 :uint8 128) (make-buffer 4)))
  (nst:def-test serialize-int8 (:array-equalp (128 129 127 255 0))
    (serialize* (:int8 0 :int8 1 :int8 -1 :int8 127 :int8 -128)
		(make-buffer 5)))
  (nst:def-test serialize-uint16 (:array-equalp (0 0  1 0  4 163  255 255))
    (serialize* (:uint16 0 :uint16 256 :uint16 1187 :uint16 65535)
		(make-buffer 8)))
  (nst:def-test serialize-int16 (:array-equalp (128   0    128   1     127 255
                                                129   0    127   0
						255 255      0   0))
    (serialize* (:int16 0 :int16 1 :int16 -1
		 :int16 256 :int16 -256
		 :int16 32767 :int16 -32768) (make-buffer 14))))
