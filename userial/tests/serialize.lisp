
(in-package :userial-tests)

;;; helper function that serializes and then unserializes a value
(defun serialize-unserialize (tag value &optional (buffer-size 64))
  "Returns the value obtained from unserializing disptached by TAG on the result of rewinding a buffer of BUFFER-SIZE bytes that had VALUE serialized into it dispatched by TAG.  The hope is that the returned value will be equal to VALUE."
  (nth-value 0
	     (unserialize tag
			  (buffer-rewind
			    (serialize tag value
				       (make-buffer buffer-size))))))

;;; an instance type of bounded arbitrary integers
(nst:def-arbitrary-instance-type (bounded-integer :key ((low 0) (high 1)))
  (assert (< low high))
  (+ (rem (abs (nst:arbitrary 'integer)) (max (- high low) 1)) low))

;;; criterion to check that we can unserialize a serialized a bounded int
(nst:def-criterion-alias (:sample-ints tag low high)
    `(:sample :sample-size 32
	      :domains ((x (bounded-integer :low ,low :high ,high)))
	      :verify (= x (serialize-unserialize ,tag x))))

;;; criterion to check that we can unserialize a serialized strings
(nst:def-criterion-alias (:sample-strings tag)
    `(:sample :sample-size 32
	      :domains ((x string))
	      :verify (equal x (serialize-unserialize ,tag x))))

;;; test various unsigned integer encode/decode routines
(nst:def-test-group test-uint-serializing ()
  (:documentation "Test the various standard unsigned-int serialize/unserialize routines.")
  (nst:def-test serialize-uint8  (:sample-ints :uint8  0 256))
  (nst:def-test serialize-uint16 (:sample-ints :uint16 0 65536))
  (nst:def-test serialize-uint24 (:sample-ints :uint24 0 16777216))
  (nst:def-test serialize-uint32 (:sample-ints :uint32 0 4294967296))
  (nst:def-test serialize-uint48 (:sample-ints :uint48 0 281474976710656))
  (nst:def-test serialize-uint64 (:sample-ints :uint64 0 18446744073709551616))
)

;;; test various signed integer encode/decode routines
(nst:def-test-group test-int-serializing ()
  (:documentation "Test the various standard signed-int serialize/unserialize routines.")
  (nst:def-test serialize-int8  (:sample-ints :int8  -128 128))
  (nst:def-test serialize-int16 (:sample-ints :int16 -32768 32768))
  (nst:def-test serialize-int32 (:sample-ints :int32 -2147483648 2147483648))
  (nst:def-test serialize-int64 (:sample-ints :int64 -9223372036854775808
					              9223372036854775808)))

;;; define a criterion that makes sure the test-form is an array of
;;; unsigned bytes that are equal to the given list of bytes
(nst:def-criterion-alias (:array-equalp bytes)
    `(:equalp ,(make-array (list (length bytes))
			   :element-type '(unsigned-byte 8)
			   :initial-contents bytes)))

;;; prepare an enum and bitfield type for testing
(make-enum-serializer     :alphas (:a :b :c :d :e :f :g :h :i :j :k :l :m
				   :n :o :p :q :r :s :t :u :v :w :x :y :z))
(make-bitfield-serializer :colors (:red :orange :yellow :green :blue
				   :indigo :violet :white :black))

;;; verify that the enums and bitfields serialize as expected
(nst:def-test-group test-enum/bitfield-serializing ()
  (:documentation "Verify that the ENUM and BITFIELD serializes serialize as expected.")
  (nst:def-test serialize-alpha-enum (:array-equalp (1 13 26))
    (serialize :alphas :z
	       (serialize :alphas :m
			  (serialize :alphas :a (make-buffer 4)))))
  (nst:def-test serialize-colors-bitfield (:array-equalp (0 0 0 1 1 2 1 255))
    (serialize :colors '(:red :orange :yellow :green :blue :indigo
			 :violet :white :black)
	       (serialize :colors '(:orange :black)
			  (serialize :colors :red
				     (serialize :colors nil
						(make-buffer 8)))))))

;;; check that strings and raw-byte arrays encode as expected
(nst:def-test-group test-string-and-byte-serializing ()
  (:documentation "Test that strings and raw-byte arrays serialize
                   as expected")
  (nst:def-test serialize-ascii-string (:array-equalp (0 3 70 111 111))
    (serialize :string "Foo" (make-buffer 5)))
  (nst:def-test serialize-utf8-string (:array-equalp (0 5 70 111 226 152 186))
    (serialize :string "Fo☺" (make-buffer 7)))
  (nst:def-test serialize-byte-array (:array-equalp (0 5 0 3 70 111 111))
    (serialize :bytes (serialize :string "Foo" (make-buffer 5))
	       (make-buffer 7)))
  #+does-not-work-yet (nst:def-test serialize-unserialize-strings
			  (:sample-strings :string))
  (nst:def-test unserialize-ascii-string (:equal "Foo")
    (serialize-unserialize :string "Foo"))
  (nst:def-test unserialize-utf8-string (:equal "Fo☺")
    (serialize-unserialize :string "Fo☺")))

;;; test serializing sequences of things
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

;;; prepare a buffer for testing unserializing
(nst:def-fixtures unserialize-buffers
    (:documentation "Prepare some buffers for unserializing")
  (buffer (buffer-rewind (serialize* (:int16 -1187 :string "Foo"
				      :uint8 3 :uint16 2178)
				     (make-buffer 64)))))

;;; test various methods of unserializing things
(nst:def-test-group test-unserialize-techniques (unserialize-buffers)
  (nst:def-test test-unserialize* (:equalp '((-1187 "Foo") 3 2178))
    (let ((a (list nil nil))
	  b
	  c)
      (nth-value 0
		 (unserialize* (:int16 (first a) :string (second a)
			        :uint8 b :uint16 c) (buffer-rewind buffer)))
      (list a b c)))
  (nst:def-test test-unserialize-let* (:equalp '(-1187 "Foo" 3 2178))
    (nth-value 0 (unserialize-let* (:int16 a :string b :uint8 c :uint16 d)
		     (buffer-rewind buffer)
		   (list a b c d))))
  (nst:def-test test-unserialize-list* (:equalp '(-1187 "Foo" 3 2178))
    (nth-value 0 (unserialize-list* '(:int16 :string :uint8 :uint16)
                                    (buffer-rewind buffer)))))
