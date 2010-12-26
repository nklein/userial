
(in-package :unet)

;;; serialize generic function
(defgeneric serialize (packet type value)
  (:documentation "Method used to serialize a VALUE of given TYPE into PACKET")
  (:method (packet type value)
    "There is no default way to serialize something, so chuck an error."
    (error "SERIALIZE not supported for type ~A" type)))

;;; unserialize generic function
(defgeneric unserialize (packet type)
  (:documentation
     "Method used to unserialize a value of given TYPE from PACKET")
  (:method (packet type)
    "There is no default way to unserialize something, so chuck an error."
    (error "UNSERIALIZE not supported for type ~A" type)))

(defmacro serialize* ((type value &rest rest) packet)
  "SERIALIZE a list of TYPE + VALUE into PACKET returning the final PACKET.  For example:  (SERIALIZE* (:UINT8 5 :INT16 -10) PACKET)"
  (if rest
      `(serialize* ,rest (serialize ,packet ,type ,value))
      `(serialize ,packet ,type ,value)))

(defmacro unserialize* ((type place &rest rest) packet &body body)
  "UNSERIALIZE a list of TYPE + PLACE from the given PACKET and execute the body.  For example:  (LET (AA BB) (UNSERIALIZE* (:UINT8 AA :INT16 BB) PACKET (LIST AA BB)))"
  (let ((pp (gensym "PLACE-"))
	(pk (gensym "PKT-")))
    `(multiple-value-bind (,pp ,pk)
	 (unserialize ,packet ,type)
       (setf ,place ,pp)
       ,@(if rest
	    `((unserialize* ,rest ,pk ,@body))
	    body))))

(defmacro unserialize-let* ((type var &rest rest) packet &body body)
  "UNSERIALIZE a list of TYPE + VARIABLE-NAME from the given PACKET and execute the body.  For example:  (UNSERIALIZE-LET* (:UINT8 AA :INT16 BB) PACKET (LIST AA BB))"
  (let ((pkt (gensym "PKT-")))
    `(multiple-value-bind (,var ,pkt)
	 (unserialize ,packet ,type)
       ,@(if rest
	     `((unserialize-let* ,rest ,pkt ,@body))
	     `((declare (ignore ,pkt)) ,@body)))))

(defun unserialize-list* (types packet)
  "UNSERIALIZE a list of types from the given PACKET into a list.  For example:  (MAPCAR #'PRINC (UNSERIALIZE-LIST* '(:UINT8 :INT16) PACKET))"
  (labels ((recurse (types packet list)
	     (cond
	       ((null types) (values (nreverse list) packet))
	       (t (multiple-value-bind (value packet)
		      (unserialize packet (first types))
		    (recurse (rest types) packet (cons value list)))))))
    (recurse types packet nil)))

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
  "fetch some number of bytes from PACKET into PLACE and OTHER-PLACES."
  (let ((byte (gensym "BYTE-"))
	(pkt  (gensym "PKT-")))
    `(multiple-value-bind (,byte ,pkt)
	 (get-byte ,packet)
       (setf ,place ,byte)
       ,(if other-places
	    `(get-bytes ,pkt ,@other-places)
	    pkt))))

;;; add-bytes-from-uint
(defmacro add-bytes-from-uint (packet value bytes)
  "Add an unsigned-int VALUE of BYTES bytes to PACKET"
  (let ((vv (gensym "VV-")))
    `(let ((,vv ,value))
       (declare (type (unsigned-byte ,(* bytes 8)) ,vv))
       (add-bytes ,packet ,@(loop :for ii :from (- bytes 1) :downto 0
			          :collecting `(ldb (byte 8 ,(* ii 8))
						    ,vv))))))

(defmacro get-bytes-as-uint (packet value bytes)
  "Fetch BYTES bytes from PACKET into some unsigned-int called VALUE."
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
  "Make SERIALIZE/UNSERIALIZE methods for an unsigned-int of BYTES bytes in length dispatched by KEY."
  `(progn
     (defmethod serialize (packet (type (eql ,key)) value)
       (declare (type packet packet)
		(ignore type)
		(type (unsigned-byte ,(* bytes 8)) value))
       (add-bytes-from-uint packet value ,bytes))
     (defmethod unserialize (packet (type (eql ,key)))
       (declare (type packet packet)
		(ignore type))
       (get-bytes-as-uint packet value ,bytes))))

;;; define standard unsigned-int serialize/deserialize methods
(make-uint-serializer :uint8  1)
(make-uint-serializer :uint16 2)
(make-uint-serializer :uint24 3)
(make-uint-serializer :uint32 4)
(make-uint-serializer :uint48 6)
(make-uint-serializer :uint64 8)

;;; signed-int serialization helper
(defmacro make-int-serializer (key bytes)
  "Make SERIALIZE/UNSERIALIZE methods for a signed-int of BYTES bytes in length dispatched by KEY."
  `(progn
     (defmethod serialize (packet (type (eql ,key)) value)
       (declare (type packet packet)
		(ignore type)
		(type (signed-byte ,(* bytes 8)) value))
       (let ((vv (+ value ,(expt 2 (- (* bytes 8) 1)))))
	 (declare (type (unsigned-byte ,(* bytes 8)) vv))
	 (add-bytes-from-uint packet vv ,bytes)))
     (defmethod unserialize (packet (type (eql ,key)))
       (declare (type packet packet)
		(ignore type))
       (multiple-value-bind (value packet)
	   (get-bytes-as-uint packet value ,bytes)
	 (declare (type (unsigned-byte ,(* bytes 8)) value)
		  (type packet packet))
	 (let ((vv (- value ,(expt 2 (- (* bytes 8) 1)))))
	   (declare (type (signed-byte ,(* bytes 8)) vv))
	   (values vv packet))))))

;;; define standard signed-int serialize/deserialize methods
(make-int-serializer :int8  1)
(make-int-serializer :int16 2)
(make-int-serializer :int32 4)
(make-int-serializer :int64 8)

;;; floating-point type helper
(defmacro make-float-serializer (key type bytes encoder decoder)
  "XXX"
  `(progn
     (defmethod serialize (packet (type (eql ,key)) value)
       (declare (type packet packet)
		(ignore type)
		(type ,type value))
       (add-bytes-from-uint packet (,encoder value) ,bytes))
     (defmethod unserialize (packet (type (eql ,key)))
       (declare (type packet packet)
		(ignore type))
       (multiple-value-bind (vv packet)
	   (get-bytes-as-uint packet vv ,bytes)
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
	   (add-bytes-from-uint packet value ,bytes)))
     (defmethod unserialize (packet (type (eql ,type)))
       (declare (type packet packet)
		(ignore type))
       (multiple-value-bind (value packet)
	   (get-bytes-as-uint packet value ,bytes)
	 (declare (type (unsigned-byte ,(* bytes 8)) value)
		  (type packet packet))
	   (let ((value (ecase value ,@(loop :for ii :from 1
					     :for vv :in choices
					     :collecting (list ii vv)))))
	     (declare (type symbol value))
	     (values value packet)))))))

(defmacro make-bitfield-serializer (type (&rest choices))
  (let ((bytes (nth-value 0 (ceiling (length choices) 8))))
    `(progn
       (defmethod serialize (packet (type (eql ,type)) (value cons))
	 (declare (type packet packet)
		  (ignore type)
		  (type cons value))
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
	   (add-bytes-from-uint packet val ,bytes)))
       (defmethod serialize (packet (type (eql ,type)) (value symbol))
	 (declare (type packet packet)
		  (ignore type)
		  (type symbol value))
	 (let ((val (ecase value
		      ,@(loop :for ii :from 0
			      :for vv :in choices
			      :collecting (list vv (expt 2 ii))))))
	   (declare (type (unsigned-byte ,(* bytes 8)) val))
	   (add-bytes-from-uint packet val ,bytes)))
     (defmethod unserialize (packet (type (eql ,type)))
       (declare (type packet packet)
		(ignore type))
       (multiple-value-bind (value packet)
	   (get-bytes-as-uint packet value ,bytes)
	 (declare (type (unsigned-byte ,(* bytes 8)) value)
		  (type packet packet))
	 (assert (< value ,(expt 2 (length choices))))
	 (values (loop :for ii :from 0 :below ,(length choices)
		       :when (plusp (logand value (expt 2 ii)))
		          :collect (svref (vector ,@choices) ii))
		 packet))))))
