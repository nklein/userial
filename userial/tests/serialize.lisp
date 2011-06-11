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

(make-alias-serializer :word :uint16)
(make-alias-serializer :dword :uint32)

;;; test various unsigned integer encode/decode routines
(nst:def-test-group test-uint-serializing ()
  (:documentation "Test the various standard unsigned-int serialize/unserialize routines.")
  (nst:def-test serialize-uint8  (:sample-ints :uint8  0 #.(expt 2 8)))
  (nst:def-test serialize-uint16 (:sample-ints :uint16 0 #.(expt 2 16)))
  (nst:def-test serialize-word   (:sample-ints :word   0 #.(expt 2 16)))
  (nst:def-test serialize-uint24 (:sample-ints :uint24 0 #.(expt 2 24)))
  (nst:def-test serialize-uint32 (:sample-ints :uint32 0 #.(expt 2 32)))
  (nst:def-test serialize-dword  (:sample-ints :dword  0 #.(expt 2 32)))
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

;;; prepare a vector type for testing
(make-vector-serializer :pixel :uint8 4)
(nst:def-test-group test-vector-serializer ()
  (nst:def-test serialize-vector (:array-equalp (3 4 5 6))
    (with-buffer (make-buffer 4)
      (serialize :pixel #(3 4 5 6))))
  
  (nst:def-test unserialize-vector (:equalp #(3 4 5 6))
    (serialize-unserialize :pixel #(3 4 5 6))))

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

(nst:def-test-group test-symbol-serializing ()
  (:documentation "Test that symbols and keywords serialize as expected")
  (nst:def-test serialize-symbol (:array-equalp (9 83 69 82 73 65 76 73 90 69
                                                 7 85 83 69 82 73 65 76))
    (with-buffer (make-buffer 18)
      (serialize :symbol 'serialize)))
  (nst:def-test unserialize-symbol (:eq 'serialize)
    (serialize-unserialize :symbol 'serialize))
  
  (nst:def-test serialize-keyword (:array-equalp (7 85 83 69 82 73 65 76))
    (with-buffer (make-buffer 8)
      (serialize :keyword :userial)))
  (nst:def-test unserialize-keyword (:eq :userial)
    (serialize-unserialize :keyword :userial)))

;;; test serializing sequences of things
(nst:def-test-group test-serialize* ()
  (nst:def-test serialize-uint8 (:array-equalp (0 255 1 128 1 0))
    (with-buffer (make-buffer 6)
      (serialize* :uint8 0 :uint8 255 :uint8 1 :uint8 128
                  :boolean t :boolean nil)))
  (nst:def-test serialize-int8 (:array-equalp (0 1 255 127 128))
    (with-buffer (make-buffer 5)
      (serialize* :int8 0 :int8 1 :int8 -1 :int8 127 :int8 -128)))
  (nst:def-test serialize-uint16 (:array-equalp (0 0  1 0  4 163  255 255))
    (with-buffer (make-buffer 8)
      (serialize* :uint16 0 :uint16 256 :uint16 1187 :uint16 65535)))
  (nst:def-test serialize-word (:array-equalp (0 0  1 0  4 163  255 255))
    (with-buffer (make-buffer 8)
      (serialize* :word 0 :word 256 :word 1187 :word 65535)))
  (nst:def-test serialize-dword (:array-equalp (0 0 0 0    0 0 1 0
                                                0 0 4 163  0 1 0 0))
    (with-buffer (make-buffer 16)
      (serialize* :dword 0 :dword 256 :dword 1187 :dword 65536)))
  (nst:def-test serialize-uint (:array-equalp (0    128 2   163 9))
    (with-buffer (make-buffer 5)
      (serialize* :uint 0 :uint 256 :uint 1187)))
  (nst:def-test serialize-int16 (:array-equalp (  0   0      0   1     255 255
                                                  1   0    255   0
                                                127 255    128   0))
    (with-buffer (make-buffer 14)
      (serialize* :int16 0 :int16 1 :int16 -1
                  :int16 256 :int16 -256
                  :int16 32767 :int16 -32768)))
  (nst:def-test serialize-int (:array-equalp (99 18  227 18))
    (with-buffer (make-buffer 5)
      (serialize* :int 1187 :int -1187))))

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

;;; testing keyed slot and accessor serializers
(defun prepare-sample-people-hash (&rest peeps)
  (let ((hash (make-hash-table :test 'equal)))
    (mapcar #'(lambda (pp)
                (destructuring-bind (n i a) pp
                  (setf (gethash n hash) (make-person :name n
                                                      :initial i
                                                      :age a))))
            peeps)
    hash))

(defun prepare-standard-people-in-hash ()
  (prepare-sample-people-hash '("Alice" :a 31)
                              '("Bob"   :b 40)
                              '("Carol" :c 37)))

(defvar *people* nil)

(defun find-person-by-name (pname)
  (gethash pname *people*))

(defun find-person-by-name-and-age (pname page)
  (let ((pp (find-person-by-name pname)))
    (when (and pp (= page (person-age pp)))
      pp)))

(nst:def-fixtures sample-people-in-hash
  (:documentation "Prepare some sample people in a hash table")
  (*people* (prepare-standard-people-in-hash)))

(make-key-slot-serializer (:change-person-attrs found-person
                             (:string name pname)
                             (find-person-by-name pname))
   :alphas initial
   :uint8 age)

(make-key-accessor-serializer (:change-person-age found-person
                                 (:string (person-name found-person) pname
                                  :uint8  (person-age found-person) page)
                                 (find-person-by-name-and-age pname page))
   :uint8 person-age)

(nst:def-test-group keyed-slot-and-accessor-serializers (sample-people-in-hash)
  (:documentation "Verify that the MAKE-KEY-SLOT-SERIALIZER and MAKE-KEY-ACCESSOR-SERIALIZER work")
  (nst:def-test serialize-with-k/s (:array-equalp (5 65 108 105 99 101 25 21))
    (with-buffer (make-buffer 8)
      (serialize :change-person-attrs (make-person :name "Alice"
                                                   :initial :z
                                                   :age 21))))
  (nst:def-test serialize-unserialize-with-k/s (:equalp '(:z 21))
    (with-buffer (make-buffer 8)
      (serialize :change-person-attrs (make-person :name "Alice"
                                                   :initial :z
                                                   :age 21))
      (buffer-rewind)
      (unserialize :change-person-attrs)
      (with-slots (initial age) (find-person-by-name "Alice")
        (list initial age))))
  
  (nst:def-test serialize-with-k/a (:array-equalp (3 66 111 98 40 40))
    (with-buffer (make-buffer 6)
      (serialize :change-person-age (make-person :name "Bob" :age 40))))
  
  (nst:def-test serialize-unserialize-with-k/a (:equal 42)
    (with-buffer (make-buffer 6)
      (serialize :string "Bob")
      (serialize :uint8 40)
      (serialize :uint8 42)
      (buffer-rewind)
      (unserialize :change-person-age)
      (person-age (find-person-by-name "Bob")))))

(defparameter *low* nil)
(defparameter *high* nil)
(make-global-variable-serializer (:cutoffs :uint) *low* *high*)

(nst:def-test-group test-global-variable-serializer ()
  (:documentation "Verify that the MAKE-GLOBAL-VARIABLE-SERIALIZER works")
  (nst:def-test serialize-global-variable
      (:array-equalp (0 10 1 100))
    (with-buffer (make-buffer 4)
      (let ((*low* 10)
            (*high* 100))
        (serialize :cutoffs '*low*)
        (serialize :cutoffs '*high*))))
  
  (nst:def-test unserialize-global-variable (:equalp '(nil nil 50 200))
    (with-buffer (make-buffer 5)
      (let ((*low* 50)
            (*high* 200))
        (serialize :cutoffs '*low*)
        (serialize :cutoffs '*high*))
      (buffer-rewind)
      (let ((aa *low*)
            (bb *high*)
            (*low* 0)
            (*high* 0))
        (unserialize :cutoffs)
        (unserialize :cutoffs)
        (list aa bb *low* *high*)))))

;; prepare for serializing possibly nil items
(make-maybe-serializer :maybe-string :string)
(make-maybe-serializer :maybe-uint32 :uint32)

(nst:def-test-group test-maybe-serializer ()
  (:documentation "Verify that the MAKE-GLOBAL-VARIABLE-SERIALIZER works")
  (nst:def-test serialize-maybe
      (:array-equalp (1 3 70 111 111 0 1 0 0 4 163 0))
    (with-buffer (make-buffer 12)
      (serialize* :maybe-string "Foo"
                  :maybe-string nil
                  :maybe-uint32 1187
                  :maybe-uint32 nil)))
  
  (nst:def-test unserialize-maybe (:equalp '("Foo" nil 1187 nil))
    (with-buffer (make-buffer 12)
      (serialize* :maybe-string "Foo"
                  :maybe-string nil
                  :maybe-uint32 1187
                  :maybe-uint32 nil)
      (buffer-rewind)
      (nth-value 0 (unserialize-list* '(:maybe-string
                                        :maybe-string
                                        :maybe-uint32
                                        :maybe-uint32))))))

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
