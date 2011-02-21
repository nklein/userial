
(in-package :userial)

(declaim (optimize (speed 3)))

;;; serialize generic function
(defgeneric serialize (type value &optional buffer)
  (:documentation "Method used to serialize a VALUE of given TYPE into BUFFER")
  (:method (type value &optional buffer)
    "There is no default way to serialize something, so chuck an error."
    (declare (ignore value buffer))
    (error "SERIALIZE not supported for type ~A" type)))

;;; unserialize generic function
(defgeneric unserialize (type &optional buffer)
  (:documentation
     "Method used to unserialize a value of given TYPE from BUFFER")
  (:method (type &optional buffer)
    "There is no default way to unserialize something, so chuck an error."
    (declare (ignore buffer))
    (error "UNSERIALIZE not supported for type ~A" type)))

(defmacro serialize* ((type value &rest rest) &optional (buffer *buffer*))
  "SERIALIZE a list of TYPE + VALUE into BUFFER returning the final BUFFER.  For example:  (SERIALIZE* (:UINT8 5 :INT16 -10) BUFFER)"
  (if rest
      `(serialize* ,rest (serialize ,type ,value ,buffer))
      `(serialize ,type ,value ,buffer)))

(defmacro unserialize* ((type place &rest rest) buffer)
  "UNSERIALIZE a list of TYPE + PLACE from the given BUFFER and execute the body.  For example:  (LET (AA BB) (UNSERIALIZE* (:UINT8 AA :INT16 BB) BUFFER (LIST AA BB)))"
  (let ((pp (gensym "PLACE-"))
	(pk (gensym "BUF-")))
    `(multiple-value-bind (,pp ,pk)
	 (unserialize ,type ,buffer)
       (values (setf ,place ,pp) ,pk)
       ,@(when rest
	    `((unserialize* ,rest ,pk))))))

(defmacro unserialize-let* ((type var &rest rest) buffer &body body)
  "UNSERIALIZE a list of TYPE + VARIABLE-NAME from the given BUFFER and execute the body.  For example:  (UNSERIALIZE-LET* (:UINT8 AA :INT16 BB) BUFFER (LIST AA BB))"
  (let ((buf (gensym "BUF-")))
    `(multiple-value-bind (,var ,buf)
	 (unserialize ,type ,buffer)
       ,(if rest
	    `(unserialize-let* ,rest ,buf ,@body)
	    `(values (progn ,@body) ,buf)))))

(defun unserialize-list* (types &optional (buffer *buffer*))
  "UNSERIALIZE a list of types from the given BUFFER into a list.  For example:  (MAPCAR #'PRINC (UNSERIALIZE-LIST* '(:UINT8 :INT16) BUFFER))"
  (labels ((recurse (types buffer list)
	     (cond
	       ((null types) (values (nreverse list) buffer))
	       (t (multiple-value-bind (value buffer)
		      (unserialize (first types) buffer)
		    (recurse (rest types) buffer (cons value list)))))))
    (recurse types buffer nil)))

;;; help build integer serialize/unserialize methods
(defmacro make-uint-serializer (key bytes)
  "Make SERIALIZE/UNSERIALIZE methods for an unsigned-int of BYTES bytes
   in length dispatched by KEY."
  `(progn
     (defmethod serialize ((type (eql ,key)) value &optional (buffer *buffer*))
       (declare (type buffer buffer)
		(ignore type)
		(type (unsigned-byte ,(* bytes 8)) value))
       (unroll-add-bytes buffer value ,bytes))
     (defmethod unserialize ((type (eql ,key)) &optional (buffer *buffer*))
       (declare (type buffer buffer)
		(ignore type))
       (values (unroll-get-bytes buffer ,bytes) buffer))))

;;; define standard unsigned-int serialize/deserialize methods
(make-uint-serializer :uint8  1)
(make-uint-serializer :uint16 2)
(make-uint-serializer :uint24 3)
(make-uint-serializer :uint32 4)
(make-uint-serializer :uint48 6)
(make-uint-serializer :uint64 8)

(defmacro make-int-serializer (key bytes)
  "Make SERIALIZE/UNSERIALIZE methods for a signed-int of BYTES bytes in length dispatched by KEY."
  `(progn
     (defmethod serialize ((type (eql ,key)) value &optional (buffer *buffer*))
       (declare (type buffer buffer)
		(ignore type)
		(type (signed-byte ,(* bytes 8)) value))
       (let ((vv (+ value ,(expt 2 (- (* bytes 8) 1)))))
	 (declare (type (unsigned-byte ,(* bytes 8)) vv))
	 (unroll-add-bytes buffer vv ,bytes)))
     (defmethod unserialize ((type (eql ,key)) &optional (buffer *buffer*))
       (declare (type buffer buffer)
		(ignore type))
       (let ((value (unroll-get-bytes buffer ,bytes)))
	 (declare (type (unsigned-byte ,(* bytes 8)) value))
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
     (defmethod serialize ((type (eql ,key)) value &optional (buffer *buffer*))
       (declare (type buffer buffer)
		(ignore type)
		(type ,type value))
       (unroll-add-bytes buffer (,encoder value) ,bytes))
     (defmethod unserialize ((type (eql ,key)) &optional (buffer *buffer*))
       (declare (type buffer buffer)
		(ignore type))
       (let ((vv (unroll-get-bytes buffer ,bytes)))
	 (declare (type (unsigned-byte ,(* bytes 8)) vv))
	 (values (,decoder vv) buffer)))))

;;; floating-point type serializers/deserializers
(make-float-serializer :float32 single-float 4
		       ieee-floats:encode-float32
		       ieee-floats:decode-float32)
(make-float-serializer :float64 double-float 8
		       ieee-floats:encode-float64
		       ieee-floats:decode-float64)

;;; byte arrays with size
(defmethod serialize ((type (eql :bytes)) value &optional (buffer *buffer*))
  "Serialize the raw bytes in the VALUE array into BUFFER"
  (declare (type buffer buffer)
	   (ignore type)
	   (type (vector (unsigned-byte 8)) value))
  (let* ((length (length value))
	 (buffer (serialize :uint16 length buffer))
	 (buffer (buffer-expand-if-needed length buffer))
	 (start  (fill-pointer buffer)))
    (incf (fill-pointer buffer) length)
    (setf (subseq buffer start (+ start length)) value)
    buffer))

(defmethod unserialize ((type (eql :bytes)) &optional (buffer *buffer*))
  "Unserialize a raw array of bytes from a BUFFER"
  (declare (type buffer buffer)
	   (ignore type))
  (multiple-value-bind (length buffer) (unserialize :uint16 buffer)
    (declare (type (unsigned-byte 16) length)
	     (type buffer buffer))
    (let* ((start  (fill-pointer buffer))
	   (buffer (buffer-advance length buffer))
	   (value  (subseq buffer start (+ start length))))
      (declare (type (vector (unsigned-byte 8)) value))
      (values value buffer))))

;;; string handling
(defmethod serialize ((type (eql :string)) value &optional (buffer *buffer*))
  "Serialize a string from VALUE into the BUFFER"
  (declare (type buffer buffer)
	   (ignore type)
	   (type string value))
  (serialize :bytes (trivial-utf-8:string-to-utf-8-bytes value) buffer))

(defmethod unserialize ((type (eql :string)) &optional (buffer *buffer*))
  "Unserialize a string from a BUFFER"
  (declare (type buffer buffer)
	   (ignore type))
  (multiple-value-bind (value buffer)
      (unserialize :bytes buffer)
    (declare (type (vector (unsigned-byte 8)) value)
	     (type buffer buffer))
    (values (trivial-utf-8:utf-8-bytes-to-string value) buffer)))

;;; enum helper
(defmacro make-enum-serializer (type (&rest choices))
  "Create serialize/unserialize methods keyed by TYPE where the possible values are given by CHOICES"
  (let ((bytes (nth-value 0 (ceiling (log (1+ (length choices)) 256)))))
    `(progn
       (defmethod serialize ((type (eql ,type)) (value (eql nil))
			     &optional (buffer *buffer*))
	 (declare (type buffer buffer)
		  (ignore type value))
	 (let ((value 0))
	   (declare (type (unsigned-byte ,(* bytes 8)) value))
	   (unroll-add-bytes buffer value ,bytes)))
       (defmethod serialize ((type (eql ,type)) value
			     &optional (buffer *buffer*))
	 (declare (type buffer buffer)
		  (ignore type)
		  (type symbol value))
	 (let ((value (ecase value ,@(loop :for ii :from 1
				           :for vv :in choices
				           :collecting (list vv ii)))))
	   (declare (type (unsigned-byte ,(* bytes 8)) value))
	   (unroll-add-bytes buffer value ,bytes)))
     (defmethod unserialize ((type (eql ,type)) &optional (buffer *buffer*))
       (declare (type buffer buffer)
		(ignore type))
       (let ((value (unroll-get-bytes buffer ,bytes)))
	 (declare (type (unsigned-byte ,(* bytes 8)) value))
	   (let ((value (ecase value ,@(loop :for ii :from 1
					     :for vv :in choices
					     :collecting (list ii vv)))))
	     (declare (type symbol value))
	     (values value buffer)))))))

(defmacro make-bitfield-serializer (type (&rest choices))
  "Create serialize/unserialize methods keyed by TYPE where the CHOICES can either be specified singly or as a list."
  (let ((bytes (nth-value 0 (ceiling (length choices) 8))))
    `(progn
       (defmethod serialize ((type (eql ,type)) (value cons)
			     &optional (buffer *buffer*))
	 (declare (type buffer buffer)
		  (ignore type)
		  (type cons value))
	 (let ((val (reduce #'(lambda (acc &optional sym)
				  (logior acc
					  (ecase sym
					    ,@(loop :for ii :from 0
						    :for vv :in choices
						    :collecting
						      (list vv (expt 2 ii))))))
			      value
			      :initial-value 0)))
	   (declare (type (unsigned-byte ,(* bytes 8)) val))
	   (unroll-add-bytes buffer val ,bytes)))
       (defmethod serialize ((type (eql ,type)) (value symbol)
			     &optional (buffer *buffer*))
	 (declare (type buffer buffer)
		  (ignore type)
		  (type symbol value))
	 (let ((val (ecase value
		      ((nil) 0)
		      ,@(loop :for ii :from 0
			      :for vv :in choices
			      :collecting (list vv (expt 2 ii))))))
	   (declare (type (unsigned-byte ,(* bytes 8)) val))
	   (unroll-add-bytes buffer val ,bytes)))
     (defmethod unserialize ((type (eql ,type)) &optional (buffer *buffer*))
       (declare (type buffer buffer)
		(ignore type))
       (let ((value (unroll-get-bytes buffer ,bytes)))
	 (declare (type (unsigned-byte ,(* bytes 8)) value))
	 (assert (< value ,(expt 2 (length choices))))
	 (values (loop :for ii :from 0 :below ,(length choices)
		       :when (plusp (logand value (expt 2 ii)))
		          :collect (svref (vector ,@choices) ii))
		 buffer))))))

(defmacro make-slot-serializer (type factory (&rest fields))
  "Make a serialize/unserialize pair with given TYPE using the FACTORY
   form when a new instance is needed where FIELDS is a list of
   key/slot values.  For example:
      (defstruct person name (age 0))
      (make-slot-serializer :person (make-person) (:string name :uint8 age))"
  (labels ((get-types-and-slots (fields &optional types slots)
	     (cond
	       ((null fields) (values (nreverse types) (nreverse slots)))
	       ((null (rest fields))
		  (error "Expected same number of TYPEs as SLOTs"))
	       (t (get-types-and-slots (rest (rest fields))
				       (cons (first fields) types)
				       (cons (second fields) slots))))))
    (multiple-value-bind (types slots) (get-types-and-slots fields)
      (let ((obj-sym (gensym "OBJ-")))
	`(progn
	   (defmethod serialize ((type (eql ,type)) value
				 &optional (buffer *buffer*))
	     (with-slots ,slots value
	       ,@(mapcar #'(lambda (type slot)
			     `(serialize ,type ,slot buffer))
			 types slots))
	     buffer)
	   (defmethod unserialize ((type (eql ,type))
				   &optional (buffer *buffer*))
	     (let ((,obj-sym ,factory))
	       (with-slots ,slots ,obj-sym
		 ,@(mapcar #'(lambda (type slot)
			       `(setf ,slot (unserialize ,type buffer)))
			   types slots))
	       (values ,obj-sym buffer))))))))

(defmacro make-accessor-serializer (type factory (&rest fields))
  "Make a serialize/unserialize pair with given TYPE using the FACTORY
   form when a new instance is needed where FIELDS is a list of
   key/accessor values.  For example:
      (defstruct person name (age 0))
      (make-slot-serializer :person (make-person)
                                    (:string person-name :uint8 person-age))"
  (labels ((get-types-and-accessors (fields &optional types slots syms)
	     (cond
	       ((null fields) (values (nreverse types)
				      (nreverse slots)
				      (nreverse syms)))
	       ((null (rest fields))
		  (error "Expected same number of TYPEs as SLOTs"))
	       (t (get-types-and-accessors (rest (rest fields))
					   (cons (first fields) types)
					   (cons (second fields) slots)
					   (cons (gensym "ACC-") syms))))))
    (multiple-value-bind (types accessors syms)
	(get-types-and-accessors fields)
      (let ((obj-sym (gensym "OBJ-")))
	`(progn
	   (defmethod serialize ((type (eql ,type)) value
				 &optional (buffer *buffer*))
	     (with-accessors ,(mapcar #'(lambda (sym accessor)
					  `(,sym ,accessor))
				      syms accessors)
		 value
	       ,@(mapcar #'(lambda (type sym)
			     `(serialize ,type ,sym buffer))
			 types syms))
	     buffer)
	   (defmethod unserialize ((type (eql ,type))
				   &optional (buffer *buffer*))
	     (let ((,obj-sym ,factory))
	       (with-accessors ,(mapcar #'(lambda (sym accessor)
					    `(,sym ,accessor))
					syms accessors)
		   ,obj-sym
		 ,@(mapcar #'(lambda (type sym)
			       `(setf ,sym (unserialize ,type buffer)))
			   types syms))
	       (values ,obj-sym buffer))))))))
