;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :userial)

(declaim (optimize (speed 3)))

;;; serialize generic function
(defgeneric serialize (type value &key buffer &allow-other-keys)
  (:documentation "Method used to serialize a VALUE of given TYPE into BUFFER")
  (:method (type value &key buffer &allow-other-keys)
    "There is no default way to serialize something, so chuck an error."
    (declare (ignore value buffer))
    (error "SERIALIZE not supported for type ~A" type)))

;;; unserialize generic function
(defgeneric unserialize (type &key buffer &allow-other-keys)
  (:documentation
     "Method used to unserialize a value of given TYPE from BUFFER")
  (:method (type &key buffer &allow-other-keys)
    "There is no default way to unserialize something, so chuck an error."
    (declare (ignore buffer))
    (error "UNSERIALIZE not supported for type ~A" type)))

(defmacro serialize* ((type value &rest rest) &key (buffer '*buffer*))
  "SERIALIZE a list of TYPE + VALUE into BUFFER returning the final BUFFER.  For example:  (SERIALIZE* (:UINT8 5 :INT16 -10) :BUFFER BUFFER)"
  (if rest
      (let ((new-buffer (gensym "BUF-")))
	`(let ((,new-buffer (serialize ,type ,value :buffer ,buffer)))
	   (serialize* ,rest :buffer ,new-buffer)))
      `(serialize ,type ,value :buffer ,buffer)))

(defmacro serialize-slots* ((&rest type-slot-plist) object
			    &key (buffer '*buffer*))
  (let ((slots (loop :for ss :in (rest type-slot-plist) :by #'cddr
		     :collecting ss)))
    `(with-slots ,slots ,object
       (serialize* ,type-slot-plist :buffer ,buffer))))

(defmacro serialize-accessors* ((&rest type-accessor-plist) object
				&key (buffer '*buffer*))
  (multiple-value-bind (vars types accessors)
      (loop :for ta :on type-accessor-plist :by #'cddr
	    :collecting (gensym "SYMS-") :into vars
	    :collecting (first ta) :into types
	    :collecting (second ta) :into accessors
	    :finally (return (values vars types accessors)))
    (flet ((mappend (fn aas bbs)
	     (reduce #'nconc (mapcar fn aas bbs))))
      `(with-accessors ,(mapcar #'(lambda (vv aa) (list vv aa)) vars accessors)
	   ,object
	 (serialize* ,(mappend #'(lambda (tt vv) (list tt vv)) types vars)
		     :buffer ,buffer)))))

(defmacro unserialize* ((type place &rest rest) &key (buffer '*buffer*))
  "UNSERIALIZE a list of TYPE + PLACE from the given BUFFER and execute the body.  For example:  (LET (AA BB) (UNSERIALIZE* (:UINT8 AA :INT16 BB) :BUFFER BUFFER (LIST AA BB)))"
  (let ((pp (gensym "PLACE-"))
	(pk (gensym "BUF-")))
    `(multiple-value-bind (,pp ,pk)
	 (unserialize ,type :buffer ,buffer)
       (values (setf ,place ,pp) ,pk)
       ,@(when rest
	    `((unserialize* ,rest :buffer ,pk))))))

(defmacro unserialize-slots* ((&rest type-slot-plist) object
			      &key (buffer '*buffer*))
  (let ((slots (loop :for ss :in (rest type-slot-plist) :by #'cddr
		     :collecting ss))
	(obj-sym (gensym "OBJECT-"))
	(buf-sym (gensym "BUFFER-")))
    `(let ((,obj-sym ,object)
	   (,buf-sym ,buffer))
       (with-slots ,slots ,obj-sym
	 (unserialize* ,type-slot-plist :buffer ,buf-sym))
       (values ,obj-sym ,buf-sym))))

(defmacro unserialize-accessors* ((&rest type-accessor-plist) object
				  &key (buffer '*buffer*))
  (multiple-value-bind (vars types accessors)
      (loop :for ta :on type-accessor-plist :by #'cddr
	    :collecting (gensym "SYMS-") :into vars
	    :collecting (first ta) :into types
	    :collecting (second ta) :into accessors
	    :finally (return (values vars types accessors)))
    (flet ((mappend (fn aas bbs)
	     (reduce #'nconc (mapcar fn aas bbs))))
      (let ((obj-sym (gensym "OBJECT-"))
	    (buf-sym (gensym "BUFFER-")))
      `(let ((,obj-sym ,object)
	     (,buf-sym ,buffer))
	 (with-accessors ,(mapcar #'(lambda (vv aa) (list vv aa))
				  vars accessors)
	     ,obj-sym
	   (unserialize* ,(mappend #'(lambda (tt vv) (list tt vv)) types vars)
			 :buffer ,buf-sym))
	 (values ,obj-sym ,buf-sym))))))

(defmacro unserialize-let* ((type var &rest rest) buffer &body body)
  "UNSERIALIZE a list of TYPE + VARIABLE-NAME from the given BUFFER and execute the body.  For example:  (UNSERIALIZE-LET* (:UINT8 AA :INT16 BB) :buffer BUFFER (LIST AA BB))"
  (let ((buf (gensym "BUF-")))
    `(multiple-value-bind (,var ,buf)
	 (unserialize ,type :buffer ,buffer)
       ,(if rest
	    `(unserialize-let* ,rest ,buf ,@body)
	    `(values (progn ,@body) ,buf)))))

(defun unserialize-list* (types &key (buffer *buffer*))
  "UNSERIALIZE a list of types from the given BUFFER into a list.  For example:  (MAPCAR #'PRINC (UNSERIALIZE-LIST* '(:UINT8 :INT16) BUFFER))"
  (labels ((recurse (types buffer list)
	     (cond
	       ((null types) (values (nreverse list) buffer))
	       (t (multiple-value-bind (value buffer)
		      (unserialize (first types) :buffer buffer)
		    (recurse (rest types) buffer (cons value list)))))))
    (recurse types buffer nil)))

;;; help build integer serialize/unserialize methods
(defmacro make-uint-serializer (key bytes)
  "Make SERIALIZE/UNSERIALIZE methods for an unsigned-int of BYTES bytes
   in length dispatched by KEY."
  `(progn
     (defmethod serialize ((type (eql ,key)) value &key (buffer *buffer*))
       (declare (type buffer buffer)
		(ignore type)
		(type (unsigned-byte ,(* bytes 8)) value))
       (unroll-add-bytes buffer value ,bytes))
     (defmethod unserialize ((type (eql ,key)) &key (buffer *buffer*))
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
     (defmethod serialize ((type (eql ,key)) value &key (buffer *buffer*))
       (declare (type buffer buffer)
		(ignore type)
		(type (signed-byte ,(* bytes 8)) value))
       (let ((vv (+ value ,(expt 2 (- (* bytes 8) 1)))))
	 (declare (type (unsigned-byte ,(* bytes 8)) vv))
	 (unroll-add-bytes buffer vv ,bytes)))
     (defmethod unserialize ((type (eql ,key)) &key (buffer *buffer*))
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
     (defmethod serialize ((type (eql ,key)) value &key (buffer *buffer*))
       (declare (type buffer buffer)
		(ignore type)
		(type ,type value))
       (unroll-add-bytes buffer (,encoder value) ,bytes))
     (defmethod unserialize ((type (eql ,key)) &key (buffer *buffer*))
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
(defmethod serialize ((type (eql :bytes)) value &key (buffer *buffer*))
  "Serialize the raw bytes in the VALUE array into BUFFER"
  (declare (type buffer buffer)
	   (ignore type)
	   (type (vector (unsigned-byte 8)) value))
  (labels ((add-bytes (value start end buffer)
	     (let* ((length    (- end start))
		    (buf-start (buffer-length :buffer buffer)))
	       (buffer-advance :amount length :buffer buffer)
	       (setf (subseq buffer buf-start (buffer-length :buffer buffer))
		     (subseq value start end))))
	   (do-segment (value start buffer)
	     (let ((end (position 237 value :start start)))
	       (cond
		 ((null end) (add-bytes value start (length value) buffer))
		 (t          (add-bytes value start (1+ end) buffer)
			     (buffer-add-byte 237 :buffer buffer)
			     (do-segment value (1+ end) buffer))))))
    (do-segment value 0 buffer)
    (buffer-add-byte 237 :buffer buffer)
    (buffer-add-byte   0 :buffer buffer)))

(defmethod unserialize ((type (eql :bytes)) &key (buffer *buffer*))
  "Unserialize a raw array of bytes from a BUFFER"
  (declare (type buffer buffer)
	   (ignore type))
  (labels ((add-segment (buffer start end output)
	     (let* ((length (- end start))
		    (out-start (buffer-length :buffer output)))
	       (buffer-advance :amount length :buffer output)
	       (setf (subseq output out-start (buffer-length :buffer output))
		     (subseq buffer start end))))
	   (do-segment (buffer start output)
	     (let ((end (position 237 buffer :start start)))
	       (case (elt buffer (1+ end))
		 (237 (add-segment buffer start (1+ end) output)
		      (do-segment buffer (+ end 2) output))
		 (  t (add-segment buffer start end output)
		      (+ end 2))))))
    (let ((output (make-buffer))
	  (start (buffer-length :buffer buffer)))
      ;; temporarily set buffer length to whole capacity
      ;; then, set buffer length to be the return of #'do-segment
      (setf (buffer-length :buffer buffer) (buffer-capacity :buffer buffer))
      (setf (buffer-length :buffer buffer) (do-segment buffer start output))
      (values output buffer))))

;;; string handling
(defmethod serialize ((type (eql :string)) value &key (buffer *buffer*))
  "Serialize a string from VALUE into the BUFFER"
  (declare (type buffer buffer)
	   (ignore type)
	   (type string value))
  (serialize :bytes (trivial-utf-8:string-to-utf-8-bytes value)
	     :buffer buffer))

(defmethod unserialize ((type (eql :string)) &key (buffer *buffer*))
  "Unserialize a string from a BUFFER"
  (declare (type buffer buffer)
	   (ignore type))
  (multiple-value-bind (value buffer)
      (unserialize :bytes :buffer buffer)
    (declare (type (vector (unsigned-byte 8)) value)
	     (type buffer buffer))
    (values (trivial-utf-8:utf-8-bytes-to-string value) buffer)))

;;; enum helper
(defmacro make-enum-serializer (type (&rest choices))
  "Create serialize/unserialize methods keyed by TYPE where the possible values are given by CHOICES"
  (let ((bytes (nth-value 0 (ceiling (log (length choices) 256)))))
    `(progn
       (defmethod serialize ((type (eql ,type)) value
			     &key (buffer *buffer*))
	 (declare (type buffer buffer)
		  (ignore type)
		  (type symbol value))
	 (let ((value (ecase value
			,@(loop :for ii :from 0
			        :for vv :in choices
			        :collecting (list (if vv vv '(nil)) ii)))))
	   (declare (type (unsigned-byte ,(* bytes 8)) value))
	   (unroll-add-bytes buffer value ,bytes)))
     (defmethod unserialize ((type (eql ,type)) &key (buffer *buffer*))
       (declare (type buffer buffer)
		(ignore type))
       (let ((value (unroll-get-bytes buffer ,bytes)))
	 (declare (type (unsigned-byte ,(* bytes 8)) value))
	   (let ((value (ecase value
			  ,@(loop :for ii :from 0
			          :for vv :in choices
			          :collecting (list ii vv)))))
	     (declare (type symbol value))
	     (values value buffer)))))))

;;; define standard enum methods
(make-enum-serializer :boolean (nil t))

(defmacro make-bitfield-serializer (type (&rest choices))
  "Create serialize/unserialize methods keyed by TYPE where the CHOICES can either be specified singly or as a list."
  (let ((bytes (nth-value 0 (ceiling (length choices) 8))))
    `(progn
       (defmethod serialize ((type (eql ,type)) (value cons)
			     &key (buffer *buffer*))
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
			     &key (buffer *buffer*))
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
     (defmethod unserialize ((type (eql ,type)) &key (buffer *buffer*))
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
      `(progn
	 (defmethod serialize ((type (eql ,type)) value
			       &key (buffer *buffer*))
	   (with-slots ,slots value
	     ,@(mapcar #'(lambda (type slot)
			   `(serialize ,type ,slot :buffer buffer))
		       types slots))
	   buffer)
	 (defmethod unserialize ((type (eql ,type))
				 &key (buffer *buffer*)
				 (object ,factory))
	   (with-slots ,slots object
	     ,@(mapcar #'(lambda (type slot)
			   `(setf ,slot (unserialize ,type
						     :buffer buffer)))
		       types slots))
	   (values object buffer))))))

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
      `(progn
	 (defmethod serialize ((type (eql ,type)) value
			       &key (buffer *buffer*))
	   (with-accessors ,(mapcar #'(lambda (sym accessor)
					`(,sym ,accessor))
				    syms accessors)
	       value
	     ,@(mapcar #'(lambda (type sym)
			   `(serialize ,type ,sym :buffer buffer))
		       types syms))
	   buffer)
	 (defmethod unserialize ((type (eql ,type))
				 &key (buffer *buffer*)
				      (object ,factory))
	   (with-accessors ,(mapcar #'(lambda (sym accessor)
					`(,sym ,accessor))
				    syms accessors) object
	     ,@(mapcar #'(lambda (type sym)
			   `(setf ,sym (unserialize ,type :buffer buffer)))
		       types syms))
	   (values object buffer))))))
