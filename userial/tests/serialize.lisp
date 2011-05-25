;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :userial-tests)

;;; helper function that serializes and then unserializes a value
(defun serialize-unserialize (tag value &optional (buffer-size 64))
  "Returns the value obtained from unserializing disptached by TAG on the result of rewinding a buffer of BUFFER-SIZE bytes that had VALUE serialized into it dispatched by TAG.  The hope is that the returned value will be equal to VALUE."
  (with-buffer (make-buffer buffer-size)
    (serialize tag value)
    (buffer-rewind)
    (nth-value 0 (unserialize tag))))

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
  (nst:def-test serialize-uint8  (:sample-ints :uint8  0 #.(expt 2 8)))
  (nst:def-test serialize-uint16 (:sample-ints :uint16 0 #.(expt 2 16)))
  (nst:def-test serialize-uint24 (:sample-ints :uint24 0 #.(expt 2 24)))
  (nst:def-test serialize-uint32 (:sample-ints :uint32 0 #.(expt 2 32)))
  (nst:def-test serialize-uint48 (:sample-ints :uint48 0 #.(expt 2 48)))
  (nst:def-test serialize-uint64 (:sample-ints :uint64 0 #.(expt 2 64)))
  (nst:def-test serialize-uint   (:sample-ints :uint   0 #.(expt 2 128))))

;;; test various signed integer encode/decode routines
(nst:def-test-group test-int-serializing ()
  (:documentation "Test the various standard signed-int serialize/unserialize routines.")
  (nst:def-test serialize-int8  (:sample-ints :int8  #.(- (expt 2 7))
                                                     #.(expt 2 7)))
  (nst:def-test serialize-int16 (:sample-ints :int16 #.(- (expt 2 15))
                                                     #.(expt 2 15)))
  (nst:def-test serialize-int32 (:sample-ints :int32 #.(- (expt 2 31))
                                                     #.(expt 2 31)))
  (nst:def-test serialize-int64 (:sample-ints :int64 #.(- (expt 2 63))
                                                     #.(expt 2 63)))
  (nst:def-test serialize-int   (:sample-ints :int   #.(- (expt 2 127))
                                                     #.(expt 2 127))))

;;; define a criterion that makes sure the test-form is an array of
;;; unsigned bytes that are equal to the given list of bytes
(defun make-ubyte-array (bytes)
  (make-array (list (length bytes))
              :element-type '(unsigned-byte 8)
              :initial-contents bytes))

(nst:def-criterion-alias (:array-equalp bytes)
    `(:equalp ,(make-ubyte-array bytes)))

;;; prepare an enum and bitfield type for testing
(make-enum-serializer     :alphas (:a :b :c :d :e :f :g :h :i :j :k :l :m
				   :n :o :p :q :r :s :t :u :v :w :x :y :z))
(make-bitfield-serializer :colors (:red :orange :yellow :green :blue
				   :indigo :violet :white :black))

;;; verify that the enums and bitfields serialize as expected
(nst:def-test-group test-enum/bitfield-serializing ()
  (:documentation "Verify that the ENUM and BITFIELD serializes serialize as expected.")
  (nst:def-test serialize-booleans (:array-equalp (1 0))
    (with-buffer (make-buffer 2)
      (serialize :boolean t)
      (serialize :boolean nil)))
  (nst:def-test serialize-boolean-nil (:eql nil)
    (serialize-unserialize :boolean nil))
  (nst:def-test serialize-boolean-t (:eql t)
    (serialize-unserialize :boolean t))
  (nst:def-test serialize-alpha-enum (:array-equalp (0 12 25))
    (with-buffer (make-buffer 3)
      (serialize :alphas :a)
      (serialize :alphas :m)
      (serialize :alphas :z)))
  (nst:def-test serialize-colors-bitfield (:array-equalp (0 0 0 1 1 2 1 255))
    (with-buffer (make-buffer 8)
      (serialize :colors nil)
      (serialize :colors :red)
      (serialize :colors '(:orange :black))
      (serialize :colors '(:red :orange :yellow :green :blue :indigo
                           :violet :white :black)))))

;;; prepare a list type for testing
(make-list-serializer :colors-list :colors)

(nst:def-test-group test-list-serializer ()
  (nst:def-test serialize-color-list (:equalp '((:red)
                                                (:green)
                                                nil
                                                (:orange :blue)))
    (serialize-unserialize :colors-list
                           (list :red
                                 (list :green)
                                 nil
                                 (list :blue :orange)))))

;;; check that strings and raw-byte arrays encode as expected
(nst:def-test-group test-string-and-byte-serializing ()
  (:documentation "Test that strings and raw-byte arrays serialize
                   as expected")
  (nst:def-test serialize-ascii-string (:array-equalp (3 70 111 111))
    (with-buffer (make-buffer 4)
      (serialize :string "Foo")))
  #+sbcl
  (nst:def-test serialize-utf8-string (:array-equalp (5 70 111 226 152 186))
    (with-buffer (make-buffer 6)
      (serialize :string "Fo☺")))
  (nst:def-test serialize-byte-array (:array-equalp
				         (4 3 70 111 111))
    (with-buffer (make-buffer 5)
      (serialize :bytes (with-buffer (make-buffer 5)
                          (serialize :string "Foo")))))
  (nst:def-test serialize-raw-byte-array (:array-equalp
                                             (3 70 111 111))
    (with-buffer (make-buffer 4)
      (serialize :raw-bytes (with-buffer (make-buffer 4)
                              (serialize :string "Foo")))))
  #+does-not-work-yet (nst:def-test serialize-unserialize-strings
			  (:sample-strings :string))
  (nst:def-test unserialize-ascii-string (:equal "Foo")
    (serialize-unserialize :string "Foo"))
  #+sbcl
  (nst:def-test unserialize-utf8-string (:equal "Fo☺")
    (serialize-unserialize :string "Fo☺"))
  (nst:def-test unserialize-raw-byte-array (:array-equalp
                                               (3 70 111 111))
    (with-buffer (make-buffer 4)
      (serialize :string "Foo")
      (buffer-rewind)
      (nth-value 0
                 (unserialize :raw-bytes
                              :output (make-array '(4)
                                                  :initial-element 0
                                                  :element-type
                                                  '(unsigned-byte 8))))))
  (nst:def-test unserialize-raw-byte-array-by-size (:array-equalp
                                                       (3 70 111 111))
    (with-buffer (make-buffer 4)
      (serialize :string "Foo")
      (buffer-rewind)
      (nth-value 0 (unserialize :raw-bytes :end 4)))))

;;; test serializing sequences of things
(nst:def-test-group test-serialize* ()
  (nst:def-test serialize-uint8 (:array-equalp (0 255 1 128 1 0))
    (with-buffer (make-buffer 6)
      (serialize* :uint8 0 :uint8 255 :uint8 1 :uint8 128
                  :boolean t :boolean nil)))
  (nst:def-test serialize-int8 (:array-equalp (128 129 127 255 0))
    (with-buffer (make-buffer 5)
      (serialize* :int8 0 :int8 1 :int8 -1 :int8 127 :int8 -128)))
  (nst:def-test serialize-uint16 (:array-equalp (0 0  1 0  4 163  255 255))
    (with-buffer (make-buffer 8)
      (serialize* :uint16 0 :uint16 256 :uint16 1187 :uint16 65535)))
  (nst:def-test serialize-int16 (:array-equalp (128   0    128   1     127 255
                                                129   0    127   0
						255 255      0   0))
    (with-buffer (make-buffer 14)
      (serialize* :int16 0 :int16 1 :int16 -1
                  :int16 256 :int16 -256
                  :int16 32767 :int16 -32768))))

;;; test slot and accessor serializing
(defstruct person
  name
  initial
  age)

(make-slot-serializer (:person-private person
                                       (make-person :name "(Unknown)"
                                                    :initial "(Unknown)"
                                                    :age "(Unknown)"))
  :string name :alphas initial :uint8 age)

(make-accessor-serializer (:person-public person
                                          (make-person :name "(Unknown)"
                                                       :initial "(Unknown)"
                                                       :age "(Unknown)"))
  :string person-name :alphas person-initial)

(nst:def-fixtures sample-people
  (:documentation "Prepare some sample people")
  (alice (make-person :name "Alice" :initial :a :age 31))
  (bob   (make-person :name "Bob" :initial :b :age 40))
  (carol (make-person :name "Carol" :initial :c  :age 37)))

(nst:def-test-group test-slot-and-accessor-serializers (sample-people)
  (nst:def-test test-private-slot-serializer (:seq (:equalp alice)
						   (:equalp bob)
						   (:equalp carol))
    (list (serialize-unserialize :person-private alice)
	  (serialize-unserialize :person-private bob)
	  (serialize-unserialize :person-private carol)))
  (nst:def-test test-public-slot-serializer (:seq (:equalp "Alice")
						  (:eql :a)
						  (:not (:equalp alice)))
    (let ((rr (serialize-unserialize :person-public alice)))
      (list (person-name rr) (person-initial rr) rr)))
  (nst:def-test test-serialize-slots* (:array-equalp (0 31))
    (with-buffer (make-buffer 2)
      (serialize-slots* alice :alphas initial :uint8 age)))
  (nst:def-test test-serialize-accessor* (:array-equalp (0 31))
    (with-buffer (make-buffer 2)
      (serialize-accessors* alice :alphas person-initial :uint8 person-age)))
  (nst:def-test test-unserialize-slots* (:equalp bob)
    (with-buffer (make-buffer)
      (serialize :person-private bob)
      (buffer-rewind)
      (nth-value 0 (unserialize-slots* (make-person)
                                       :string name
                                       :alphas initial
                                       :uint8  age))))
  (nst:def-test test-unserialize-accessors* (:equalp bob)
    (with-buffer (make-buffer)
      (serialize :person-private bob)
      (buffer-rewind)
      (nth-value 0 (unserialize-accessors* (make-person)
                                           :string person-name
                                           :alphas person-initial
                                           :uint8  person-age)))))
    

;;; prepare a buffer for testing unserializing
(nst:def-fixtures unserialize-buffers
    (:documentation "Prepare some buffers for unserializing")
  (ubuffer (with-buffer (make-buffer 64)
             (serialize* :int16 -1187 :string "Foo"
                         :uint8 3 :uint16 2178)
             (buffer-rewind))))

;;; test various methods of unserializing things
(nst:def-test-group test-unserialize-techniques (unserialize-buffers)
  (nst:def-test test-unserialize* (:equalp '((-1187 "Foo") 3 2178))
    (with-buffer ubuffer
      (buffer-rewind)
      (let ((a (list nil nil))
            b
            c)
        (unserialize* :int16 (first a) :string (second a)
                      :uint8 b :uint16 c)
        (list a b c))))
  (nst:def-test test-unserialize-let* (:equalp '(-1187 "Foo" 3 2178))
    (with-buffer ubuffer
      (buffer-rewind)
      (nth-value 0 (unserialize-let* (:int16 a :string b :uint8 c :uint16 d)
                     (list a b c d)))))
  (nst:def-test test-unserialize-list* (:equalp '(-1187 "Foo" 3 2178))
    (with-buffer ubuffer
      (buffer-rewind)
      (nth-value 0 (unserialize-list* '(:int16 :string :uint8 :uint16))))))

;;; testing versioned serializers
(contextl:deflayer v1)
(contextl:deflayer v0.1)
(contextl:deflayer v1.1 (v1))
(contextl:deflayer v2.0 (v1))
(make-enum-serializer :difficulty (:low :high))
(make-enum-serializer :difficulty (:low :medium :high) :layer v1.1)
(make-enum-serializer :difficulty (:easy :hard) :layer v2.0)

(nst:def-test-group test-layered-versions (unserialize-buffers)
  (nst:def-test test-serializing-versions (:array-equalp (0 1 0 1 2 0 1))
    (with-buffer (make-buffer 7)
      (contextl:with-active-layers ()
        (serialize* :difficulty :low :difficulty :high))
      (contextl:with-active-layers (v1.1)
        (serialize* :difficulty :low :difficulty :medium :difficulty :high))
      (contextl:with-active-layers (v2.0)
        (serialize* :difficulty :easy :difficulty :hard))))
  (nst:def-test test-unserializing-versions (:equalp '(:low :high
                                                       :low :medium :high
                                                       :easy :hard))
    (with-buffer (make-buffer 7)
      (serialize :raw-bytes (make-ubyte-array '(0 1 0 1 2 0 1)))
      (buffer-rewind)
      (apply #'append
             (list (contextl:with-active-layers ()
                     (unserialize-list* '(:difficulty :difficulty)))
                   (contextl:with-active-layers (v1.1)
                     (unserialize-list* '(:difficulty
                                          :difficulty
                                          :difficulty)))
                   (contextl:with-active-layers (v2.0)
                     (unserialize-list* '(:difficulty :difficulty))))))))
