
(in-package :userial)

;;; serialize generic function
(defgeneric serialize (buffer type value)
  (:documentation "Method used to serialize a VALUE of given TYPE into BUFFER")
  (:method (buffer type value)
    "There is no default way to serialize something, so chuck an error."
    (error "SERIALIZE not supported for type ~A" type)))

;;; unserialize generic function
(defgeneric unserialize (buffer type)
  (:documentation
     "Method used to unserialize a value of given TYPE from BUFFER")
  (:method (buffer type)
    "There is no default way to unserialize something, so chuck an error."
    (error "UNSERIALIZE not supported for type ~A" type)))

(defmacro serialize* ((type value &rest rest) buffer)
  "SERIALIZE a list of TYPE + VALUE into BUFFER returning the final BUFFER.  For example:  (SERIALIZE* (:UINT8 5 :INT16 -10) BUFFER)"
  (if rest
      `(serialize* ,rest (serialize ,buffer ,type ,value))
      `(serialize ,buffer ,type ,value)))

(defmacro unserialize* ((type place &rest rest) buffer &body body)
  "UNSERIALIZE a list of TYPE + PLACE from the given BUFFER and execute the body.  For example:  (LET (AA BB) (UNSERIALIZE* (:UINT8 AA :INT16 BB) BUFFER (LIST AA BB)))"
  (let ((pp (gensym "PLACE-"))
	(pk (gensym "PKT-")))
    `(multiple-value-bind (,pp ,pk)
	 (unserialize ,buffer ,type)
       ,(unless rest
		`(declare (ignore ,pk)))
       (setf ,place ,pp)
       ,@(if rest
	    `((unserialize* ,rest ,pk ,@body))
	    body))))

(defmacro unserialize-let* ((type var &rest rest) buffer &body body)
  "UNSERIALIZE a list of TYPE + VARIABLE-NAME from the given BUFFER and execute the body.  For example:  (UNSERIALIZE-LET* (:UINT8 AA :INT16 BB) BUFFER (LIST AA BB))"
  (let ((pkt (gensym "PKT-")))
    `(multiple-value-bind (,var ,pkt)
	 (unserialize ,buffer ,type)
       ,@(if rest
	     `((unserialize-let* ,rest ,pkt ,@body))
	     `((declare (ignore ,pkt)) ,@body)))))

(defun unserialize-list* (types buffer)
  "UNSERIALIZE a list of types from the given BUFFER into a list.  For example:  (MAPCAR #'PRINC (UNSERIALIZE-LIST* '(:UINT8 :INT16) BUFFER))"
  (labels ((recurse (types buffer list)
	     (cond
	       ((null types) (values (nreverse list) buffer))
	       (t (multiple-value-bind (value buffer)
		      (unserialize buffer (first types))
		    (recurse (rest types) buffer (cons value list)))))))
    (recurse types buffer nil)))

;;; add-byte function
(declaim (ftype (function (buffer (unsigned-byte 8)) buffer) add-byte))
(defun add-byte (buffer byte)
  "Add a given BYTE to a BUFFER"
  (declare (type buffer buffer)
	   (type (unsigned-byte 8) byte))
  (setf (aref buffer (fill-pointer buffer)) byte)
  (incf (fill-pointer buffer))
  buffer)

;;; add-bytes macro
(defmacro add-bytes (buffer byte &rest other-bytes)
  "Add some number of bytes to BUFFER."
  (cond
    ((null other-bytes) `(add-byte ,buffer ,byte))
    (t                  `(add-bytes (add-bytes ,buffer ,byte) ,@other-bytes))))

;;; get-byte function
(declaim (ftype (function (buffer) (values (unsigned-byte 8) buffer))
		get-byte))
(defun get-byte (buffer)
  "Get a byte from a BUFFER"
  (declare (type buffer buffer))
  (let ((byte (aref buffer (fill-pointer buffer)))
	(buffer (make-displaced-buffer buffer (1+ (fill-pointer buffer)))))
      (declare (type (unsigned-byte 8) byte)
	       (type buffer buffer))
      (values byte buffer)))

;;; get-bytes macro
(defmacro get-bytes (buffer place &rest other-places)
  "fetch some number of bytes from BUFFER into PLACE and OTHER-PLACES."
  (let ((byte (gensym "BYTE-"))
	(pkt  (gensym "PKT-")))
    `(multiple-value-bind (,byte ,pkt)
	 (get-byte ,buffer)
       (setf ,place ,byte)
       ,(if other-places
	    `(get-bytes ,pkt ,@other-places)
	    pkt))))

;;; add-bytes-from-uint
(defmacro add-bytes-from-uint (buffer value bytes)
  "Add an unsigned-int VALUE of BYTES bytes to BUFFER"
  (let ((vv (gensym "VV-")))
    `(let ((,vv ,value))
       (declare (type (unsigned-byte ,(* bytes 8)) ,vv))
       (add-bytes ,buffer ,@(loop :for ii :from (- bytes 1) :downto 0
			          :collecting `(ldb (byte 8 ,(* ii 8))
						    ,vv))))))

(defmacro get-bytes-as-uint (buffer value bytes)
  "Fetch BYTES bytes from BUFFER into some unsigned-int called VALUE."
  (let ((pkt (gensym "PKT-")))
    `(let ((,value 0)
	   (,pkt ,buffer))
       (declare	(type (unsigned-byte ,(* bytes 8)) ,value)
		(type buffer ,pkt))
       (let ((,pkt (get-bytes ,pkt
			      ,@(loop :for ii :from (- bytes 1) :downto 0
				      :collecting `(ldb (byte 8 ,(* ii 8))
							,value)))))
	 (declare (type buffer ,pkt))
	 (values ,value ,pkt)))))

;;; help build integer serialize/unserialize methods
(defmacro make-uint-serializer (key bytes)
  "Make SERIALIZE/UNSERIALIZE methods for an unsigned-int of BYTES bytes in length dispatched by KEY."
  `(progn
     (defmethod serialize (buffer (type (eql ,key)) value)
       (declare (type buffer buffer)
		(ignore type)
		(type (unsigned-byte ,(* bytes 8)) value))
       (add-bytes-from-uint buffer value ,bytes))
     (defmethod unserialize (buffer (type (eql ,key)))
       (declare (type buffer buffer)
		(ignore type))
       (get-bytes-as-uint buffer value ,bytes))))

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
     (defmethod serialize (buffer (type (eql ,key)) value)
       (declare (type buffer buffer)
		(ignore type)
		(type (signed-byte ,(* bytes 8)) value))
       (let ((vv (+ value ,(expt 2 (- (* bytes 8) 1)))))
	 (declare (type (unsigned-byte ,(* bytes 8)) vv))
	 (add-bytes-from-uint buffer vv ,bytes)))
     (defmethod unserialize (buffer (type (eql ,key)))
       (declare (type buffer buffer)
		(ignore type))
       (multiple-value-bind (value buffer)
	   (get-bytes-as-uint buffer value ,bytes)
	 (declare (type (unsigned-byte ,(* bytes 8)) value)
		  (type buffer buffer))
	 (let ((vv (- value ,(expt 2 (- (* bytes 8) 1)))))
	   (declare (type (signed-byte ,(* bytes 8)) vv))
	   (values vv buffer))))))

;;; define standard signed-int serialize/deserialize methods
(make-int-serializer :int8  1)
(make-int-serializer :int16 2)
(make-int-serializer :int32 4)
(make-int-serializer :int64 8)

;;; floating-point type helper
(defmacro make-float-serializer (key type bytes encoder decoder)
  "Make serialize/unserialize routines for floating-point type TYPE dispatched by KEY with the given number of BYTES and an ENCODER/DECODER pair."
  `(progn
     (defmethod serialize (buffer (type (eql ,key)) value)
       (declare (type buffer buffer)
		(ignore type)
		(type ,type value))
       (add-bytes-from-uint buffer (,encoder value) ,bytes))
     (defmethod unserialize (buffer (type (eql ,key)))
       (declare (type buffer buffer)
		(ignore type))
       (multiple-value-bind (vv buffer)
	   (get-bytes-as-uint buffer vv ,bytes)
	 (declare (type (unsigned-byte ,(* bytes 8)) vv)
		  (type buffer buffer))
	 (values (,decoder vv) buffer)))))

;;; floating-point type serializers/deserializers
(make-float-serializer :float32 single-float 4
		       ieee-floats:encode-float32
		       ieee-floats:decode-float32)
(make-float-serializer :float64 double-float 8
		       ieee-floats:encode-float64
		       ieee-floats:decode-float64)

;;; raw byte arrays
(defmethod serialize (buffer (type (eql :bytes)) value)
  "Serialize the raw bytes in the VALUE array into BUFFER"
  (declare (type buffer buffer)
	   (ignore type)
	   (type (vector (unsigned-byte 8)) value))
  (let* ((length (length value))
	 (buffer (serialize buffer :uint16 length))
	 (start  (fill-pointer buffer)))
    (incf (fill-pointer buffer) length)
    (setf (subseq buffer start (+ start length)) value)
    buffer))

(defmethod unserialize (buffer (type (eql :bytes)))
  "Unserialize a raw array of bytes from a BUFFER"
  (declare (type buffer buffer)
	   (ignore type))
  (multiple-value-bind (length buffer) (unserialize buffer :uint16)
    (declare (type (unsigned-byte 16) length)
	     (type buffer buffer))
    (let* ((start  (fill-pointer buffer))
	   (buffer (make-displaced-buffer buffer (+ start length)))
	   (value  (subseq buffer start (+ start length))))
      (declare (type (vector (unsigned-byte 8)) value))
      (values value buffer))))

;;; string handling
(defmethod serialize (buffer (type (eql :string)) value)
  "Serialize a string from VALUE into the BUFFER"
  (declare (type buffer buffer)
	   (ignore type)
	   (type string value))
  (serialize buffer :bytes (trivial-utf-8:string-to-utf-8-bytes value)))

(defmethod unserialize (buffer (type (eql :string)))
  "Unserialize a string from a BUFFER"
  (declare (type buffer buffer)
	   (ignore type))
  (multiple-value-bind (value buffer)
      (unserialize buffer :bytes)
    (declare (type (vector (unsigned-byte 8)) value)
	     (type buffer buffer))
    (values (trivial-utf-8:utf-8-bytes-to-string value) buffer)))

;;; enum helper
(defmacro make-enum-serializer (type (&rest choices))
  "Create serialize/unserialize methods keyed by TYPE where the possible values are given by CHOICES"
  (let ((bytes (nth-value 0 (ceiling (log (1+ (length choices)) 256)))))
    `(progn
       (defmethod serialize (buffer (type (eql ,type)) (value (eql nil)))
	 (declare (type buffer buffer)
		  (ignore type value))
	 (let ((value 0))
	   (declare (type (unsigned-byte ,(* bytes 8)) value))
	   (add-bytes-from-uint buffer value ,bytes)))
       (defmethod serialize (buffer (type (eql ,type)) value)
	 (declare (type buffer buffer)
		  (ignore type)
		  (type symbol value))
	 (let ((value (ecase value ,@(loop :for ii :from 1
				           :for vv :in choices
				           :collecting (list vv ii)))))
	   (declare (type (unsigned-byte ,(* bytes 8)) value))
	   (add-bytes-from-uint buffer value ,bytes)))
     (defmethod unserialize (buffer (type (eql ,type)))
       (declare (type buffer buffer)
		(ignore type))
       (multiple-value-bind (value buffer)
	   (get-bytes-as-uint buffer value ,bytes)
	 (declare (type (unsigned-byte ,(* bytes 8)) value)
		  (type buffer buffer))
	   (let ((value (ecase value ,@(loop :for ii :from 1
					     :for vv :in choices
					     :collecting (list ii vv)))))
	     (declare (type symbol value))
	     (values value buffer)))))))

(defmacro make-bitfield-serializer (type (&rest choices))
  "Create serialize/unserialize methods keyed by TYPE where the CHOICES can either be specified singly or as a list."
  (let ((bytes (nth-value 0 (ceiling (length choices) 8))))
    `(progn
       (defmethod serialize (buffer (type (eql ,type)) (value cons))
	 (declare (type buffer buffer)
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
	   (add-bytes-from-uint buffer val ,bytes)))
       (defmethod serialize (buffer (type (eql ,type)) (value symbol))
	 (declare (type buffer buffer)
		  (ignore type)
		  (type symbol value))
	 (let ((val (ecase value
		      ,@(loop :for ii :from 0
			      :for vv :in choices
			      :collecting (list vv (expt 2 ii))))))
	   (declare (type (unsigned-byte ,(* bytes 8)) val))
	   (add-bytes-from-uint buffer val ,bytes)))
     (defmethod unserialize (buffer (type (eql ,type)))
       (declare (type buffer buffer)
		(ignore type))
       (multiple-value-bind (value buffer)
	   (get-bytes-as-uint buffer value ,bytes)
	 (declare (type (unsigned-byte ,(* bytes 8)) value)
		  (type buffer buffer))
	 (assert (< value ,(expt 2 (length choices))))
	 (values (loop :for ii :from 0 :below ,(length choices)
		       :when (plusp (logand value (expt 2 ii)))
		          :collect (svref (vector ,@choices) ii))
		 buffer))))))
