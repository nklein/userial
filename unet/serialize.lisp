
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

;;; add-byte-helper
(defmacro add-bytes-helper (packet value bytes)
  "add an unsigned-int VALUE of BYTES bytes to PACKET"
  (let ((vv (gensym "VV-")))
    `(let ((,vv ,value))
       (declare (type (unsigned-byte ,(* bytes 8)) ,vv))
       (add-bytes ,packet ,@(loop :for ii :from (- bytes 1) :downto 0
			          :collecting `(ldb (byte 8 ,(* ii 8))
						    ,vv))))))

(defmacro get-bytes-helper (packet value bytes)
  (let ((pkt (gensym "PKT-")))
    `(let ((,value 0)
	   (,pkt ,packet))
       (declare	(type (unsigned-byte ,(* bytes 8)) ,value)
		(type packet ,pkt))
       (let ((,pkt (get-bytes ,pkt
			      ,@(loop :for ii :from (- bytes 1) :downto 0
				      :collecting `(ldb (byte 8 ,(* ii 8))
							,value)))))
	 (declare (type packet ,pkt))
	 (values ,value ,pkt)))))

;;; help build integer serialize/unserialize methods
(defmacro make-uint-serializer (key bytes)
  `(progn
     (defmethod serialize (packet (type (eql ,key)) value)
       (declare (type packet packet)
		(ignore type)
		(type (unsigned-byte ,(* bytes 8)) value))
       (add-bytes-helper packet value ,bytes))
     (defmethod unserialize (packet (type (eql ,key)))
       (declare (type packet packet)
		(ignore type))
       (get-bytes-helper packet value ,bytes))))

;;; unsigned-int serialization
(make-uint-serializer :uint8  1)
(make-uint-serializer :uint16 2)
(make-uint-serializer :uint24 3)
(make-uint-serializer :uint32 4)
(make-uint-serializer :uint48 6)
(make-uint-serializer :uint64 8)

;;; signed-int serialization helper
(defmacro make-int-serializer (key bytes)
  `(progn
     (defmethod serialize (packet (type (eql ,key)) value)
       (declare (type packet packet)
		(ignore type)
		(type (signed-byte ,(* bytes 8)) value))
       (let ((vv (+ value ,(expt 2 (- (* bytes 8) 1)))))
	 (declare (type (unsigned-byte ,(* bytes 8)) vv))
	 (add-bytes-helper packet vv ,bytes)))
     (defmethod unserialize (packet (type (eql ,key)))
       (declare (type packet packet)
		(ignore type))
       (multiple-value-bind (value packet)
	   (get-bytes-helper packet value ,bytes)
	 (declare (type (unsigned-byte ,(* bytes 8)) value)
		  (type packet packet))
	 (let ((vv (- value ,(expt 2 (- (* bytes 8) 1)))))
	   (declare (type (signed-byte ,(* bytes 8)) vv))
	   (values vv packet))))))

;;; signed-int serialization methods
(make-int-serializer :int8  1)
(make-int-serializer :int16 2)
(make-int-serializer :int32 4)
(make-int-serializer :int48 6)
(make-int-serializer :int64 8)

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
(defmacro make-float-serializer (key type bytes encoder decoder)
  `(progn
     (defmethod serialize (packet (type (eql ,key)) value)
       (declare (type packet packet)
		(ignore type)
		(type ,type value))
       (add-bytes-helper packet (,encoder value) ,bytes))
     (defmethod unserialize (packet (type (eql ,key)))
       (declare (type packet packet)
		(ignore type))
       (multiple-value-bind (vv packet)
	   (get-bytes-helper packet vv ,bytes)
	 (declare (type (unsigned-byte ,(* bytes 8)) vv)
		  (type packet packet))
	 (values (,decoder vv) packet)))))

;;; floating-point type serializers/deserializers
(make-float-serializer :float32 single-float 4
		       ieee-floats:encode-float32
		       ieee-floats:decode-float32)
(make-float-serializer :float64 double-float 8
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

;;; enum helper
(defmacro make-enum-serializer (type (&rest choices))
  (let ((bytes (nth-value 0 (ceiling (log (1+ (length choices)) 256)))))
    `(progn
       (defmethod serialize (packet (type (eql ,type)) value)
	 (declare (type packet packet)
		  (ignore type)
		  (type symbol value))
	 (let ((value (ecase value ,@(loop :for ii :from 1
				           :for vv :in choices
				           :collecting (list vv ii)))))
	   (declare (type (unsigned-byte ,(* bytes 8)) value))
	   (add-bytes-helper packet value ,bytes)))
     (defmethod unserialize (packet (type (eql ,type)))
       (declare (type packet packet)
		(ignore type))
       (multiple-value-bind (value packet)
	   (get-bytes-helper packet value ,bytes)
	 (declare (type (unsigned-byte ,(* bytes 8)) value)
		  (type packet packet))
	   (let ((value (ecase value ,@(loop :for ii :from 1
					     :for vv :in choices
					     :collecting (list ii vv)))))
	     (declare (type symbol value))
	     (values value packet)))))))

(defmacro make-bitfield-serializer (type (&rest choices))
  (let ((bytes (nth-value 0 (ceiling (length choices) 256))))
    `(progn
       (defmethod serialize (packet (type (eql ,type)) value)
	 (declare (type packet packet)
		  (ignore type)
		  (type (vector symbol) value))
	 (let ((val (reduce #'(lambda (acc sym)
				  (logior acc
					  (ecase sym
					    ,@(loop :for ii :from 0
						    :for vv :in choices
						    :collecting
						      (list vv (expt 2 ii))))))
			      value
			      :initial-value 0)))
	   (declare (type (unsigned-byte ,(* bytes 8)) val))
	   (add-bytes-helper packet val ,bytes)))
     (defmethod unserialize (packet (type (eql ,type)))
       (declare (type packet packet)
		(ignore type))
       (multiple-value-bind (value packet)
	   (get-bytes-helper packet value ,bytes)
	 (declare (type (unsigned-byte ,(* bytes 8)) value)
		  (type packet packet))
	 (assert (< value ,(expt 2 (length choices))))
	 (values (loop :for ii :from 0 :below ,(length choices)
		       :when (plusp (logand value (expt 2 ii)))
		          :collect (svref (vector ,@choices) ii))
		 packet))))))
