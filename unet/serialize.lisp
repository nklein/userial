
(in-package :unet)

;;; serialize generic function
(defgeneric serialize (packet type value)
  (:documentation "Method used to serialize a VALUE of given TYPE into PACKET")
  (:method (packet type value)
    (error "SERIALIZE not supported for type ~A" type)))

;;; unserialize generic function
(defgeneric unserialize (packet type)
  (:documentation
     "Method used to unserialize a value of given TYPE from PACKET")
  (:method (packet type)
    (error "UNSERIALIZE not supported for type ~A" type)))

;;; add-byte function
(declaim (ftype (function (packet (unsigned-byte 8)) packet) add-byte))
(defun add-byte (packet byte)
  "Add a given BYTE to a PACKET"
  (declare (type packet packet)
	   (type (unsigned-byte 8) byte))
  (setf (aref packet (fill-pointer packet)) byte)
  (incf (fill-pointer packet))
  packet)

;;; add-bytes macro
(defmacro add-bytes (packet byte &rest other-bytes)
  "Add some number of bytes to PACKET."
  (cond
    ((null other-bytes) `(add-byte ,packet ,byte))
    (t                  `(add-bytes (add-bytes ,packet ,byte) ,@other-bytes))))

;;; get-byte function
(declaim (ftype (function (packet) (values (unsigned-byte 8) packet))
		get-byte))
(defun get-byte (packet)
  "Get a byte from a PACKET"
  (declare (type packet packet))
  (let ((byte (aref packet (fill-pointer packet)))
	(packet (make-displaced-packet packet (1+ (fill-pointer packet)))))
      (declare (type (unsigned-byte 8) byte)
	       (type packet packet))
      (values byte packet)))

;;; get-bytes macro
(defmacro get-bytes (packet place &rest other-places)
  "Add some number of bytes from PACKET."
  (let ((byte (gensym "BYTE-"))
	(pkt  (gensym "PKT-")))
    `(multiple-value-bind (,byte ,pkt)
	 (get-byte ,packet)
       (setf ,place ,byte)
       ,(if other-places
	    `(get-bytes ,pkt ,@other-places)
	    pkt))))

;;; help build integer serialize/unserialize methods
(defmacro make-uint-serializer (key bytes)
  `(progn
     (defmethod serialize (packet (type (eql ,key)) value)
       (declare (type packet packet)
		(ignore type)
		(type (unsigned-byte ,(* bytes 8)) value))
       (add-bytes packet
		  ,@(loop :for ii :from (- bytes 1) :downto 0
		       :collecting `(ldb (byte 8 ,(* ii 8)) value))))
     (defmethod unserialize (packet (type (eql ,key)))
       (declare (type packet packet)
		(ignore type))
       (let ((value 0))
	 (declare (type (unsigned-byte ,(* bytes 8)) value))
	 (let ((packet (get-bytes packet
				  ,@(loop :for ii :from (- bytes 1) :downto 0
				       :collecting
				       `(ldb (byte 8 ,(* ii 8)) value)))))
	   (declare (type packet packet))
	   (values value packet))))))

;;; unsigned-int serialization
(make-uint-serializer :uint8  1)
(make-uint-serializer :uint16 2)
(make-uint-serializer :uint24 3)
(make-uint-serializer :uint32 4)
(make-uint-serializer :uint48 6)
(make-uint-serializer :uint64 8)

;;; signed-int serialization helper
(defmacro make-int-serializer (key ukey bytes)
  `(progn
     (defmethod serialize (packet (type (eql ,key)) value)
       (declare (type packet packet)
		(ignore type)
		(type (signed-byte ,(* bytes 8)) value))
       (let ((vv (+ value ,(expt 2 (- (* bytes 8) 1)))))
	 (declare (type (unsigned-byte ,(* bytes 8)) vv))
	 (serialize packet ,ukey vv)))
     (defmethod unserialize (packet (type (eql ,key)))
       (declare (type packet packet)
		(ignore type))
       (multiple-value-bind (value packet) (unserialize packet ,ukey)
	 (declare (type (unsigned-byte ,(* bytes 8)) value)
		  (type packet packet))
	 (let ((vv (- value ,(expt 2 (- (* bytes 8) 1)))))
	   (declare (type (signed-byte ,(* bytes 8)) vv))
	   (values vv packet))))))

;;; signed-int serialization methods
(make-int-serializer :int8  :uint8  1)
(make-int-serializer :int16 :uint16 2)
(make-int-serializer :int32 :uint32 4)
(make-int-serializer :int48 :uint48 6)
(make-int-serializer :int64 :uint64 8)

(defmacro serialize* ((type value &rest rest) packet)
  (if rest
      `(serialize* ,rest (serialize ,packet ,type ,value))
      `(serialize ,packet ,type ,value)))

(defmacro unserialize* ((type place &rest rest) packet &body body)
  (let ((pp (gensym "PLACE-"))
	(pk (gensym "PKT-")))
    `(multiple-value-bind (,pp ,pk)
	 (unserialize ,packet ,type)
       (setf ,place ,pp)
       ,@(if rest
	    `((unserialize* ,rest ,pk ,@body))
	    body))))

(defmacro unserialize-let* ((type place &rest rest) packet &body body)
  (let ((pkt (gensym "PKT-")))
    `(multiple-value-bind (,place ,pkt)
	 (unserialize ,packet ,type)
       ,@(if rest
	     `((unserialize-let* ,rest ,pkt ,@body))
	     `((declare (ignore ,pkt)) ,@body)))))

;;; floating-point type helper
(defmacro make-float-serializer (key int-key type int-type encoder decoder)
  `(progn
     (defmethod serialize (packet (type (eql ,key)) value)
       (declare (type packet packet)
		(ignore type)
		(type ,type value))
       (serialize packet ,int-key (,encoder value)))
     (defmethod unserialize (packet (type (eql ,key)))
       (declare (type packet packet)
		(ignore type))
       (multiple-value-bind (vv packet)
	   (unserialize packet ,int-key)
	 (declare (type ,int-type vv)
		  (type packet packet))
	 (values (,decoder vv) packet)))))

;;; floating-point type serializers/deserializers
(make-float-serializer :float32     :uint32
		       single-float (unsigned-byte 32)
		       ieee-floats:encode-float32
		       ieee-floats:decode-float32)
(make-float-serializer :float64     :uint64
		       double-float (unsigned-byte 64)
		       ieee-floats:encode-float64
		       ieee-floats:decode-float64)

;;; raw byte arrays
(defmethod serialize (packet (type (eql :bytes)) value)
  (declare (type packet packet)
	   (ignore type)
	   (type (vector (unsigned-byte 8)) value))
  (let* ((length (length value))
	 (packet (serialize packet :uint16 length))
	 (start  (fill-pointer packet)))
    (incf (fill-pointer packet) length)
    (setf (subseq packet start (+ start length)) value)
    packet))


(defmethod unserialize (packet (type (eql :bytes)))
  (declare (type packet packet)
	   (ignore type))
  (multiple-value-bind (length packet) (unserialize packet :uint16)
    (declare (type (unsigned-byte 16) length)
	     (type packet packet))
    (let* ((start  (fill-pointer packet))
	   (packet (make-displaced-packet packet (+ start length)))
	   (value  (subseq packet start (+ start length))))
      (declare (type (vector (unsigned-byte 8)) value))
      (values value packet))))

;;; string handling
(defmethod serialize (packet (type (eql :string)) value)
  (declare (type packet packet)
	   (ignore type)
	   (type string value))
  (serialize packet :bytes (trivial-utf-8:string-to-utf-8-bytes value)))

(defmethod unserialize (packet (type (eql :string)))
  (declare (type packet packet)
	   (ignore type))
  (multiple-value-bind (value packet)
      (unserialize packet :bytes)
    (declare (type (vector (unsigned-byte 8)) value)
	     (type packet packet))
    (values (trivial-utf-8:utf-8-bytes-to-string value) packet)))