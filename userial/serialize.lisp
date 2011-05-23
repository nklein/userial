;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :userial)

;;; serialize generic function
(contextl:define-layered-function serialize
    (type value &key buffer &allow-other-keys)
  (:documentation "Method used to serialize a VALUE of given TYPE into BUFFER")
  (:method (type value &key buffer &allow-other-keys)
    "There is no default way to serialize something, so chuck an error."
    (declare (ignore value buffer))
    (error "SERIALIZE not supported for type ~A" type)))

;;; unserialize generic function
(contextl:define-layered-function unserialize
    (type &key buffer &allow-other-keys)
  (:documentation
     "Method used to unserialize a value of given TYPE from BUFFER")
  (:method (type &key buffer &allow-other-keys)
    "There is no default way to unserialize something, so chuck an error."
    (declare (ignore buffer))
    (error "UNSERIALIZE not supported for type ~A" type)))

(defmacro serialize* ((&rest type-value-pairs) &key (buffer '*buffer*))
  "SERIALIZE a list of TYPE + VALUE into BUFFER returning the final BUFFER.  For example:  (SERIALIZE* (:UINT8 5 :INT16 -10) :BUFFER BUFFER)"
  (multiple-value-bind (vars types values)
      (get-syms-types-places type-value-pairs)
    (declare (ignore vars))
    (let ((buf (gensym "BUF-")))
      `(let* ((,buf ,buffer)
              ,@(mapcar #'(lambda (tt vv)
                            `(,buf
                               (serialize ,tt ,vv :buffer ,buf)))
                        types values))
         ,buf))))

(defmacro serialize-slots* ((&rest type-slot-plist) object
			    &key (buffer '*buffer*))
  (multiple-value-bind (vars types slots)
      (get-syms-types-places type-slot-plist)
    `(with-slots ,(mapcar #'quote-2 vars slots) ,object
         (serialize* ,(apply #'append (mapcar #'quote-2 types vars))
                     :buffer ,buffer))))

(defmacro serialize-accessors* ((&rest type-accessor-plist) object
				&key (buffer '*buffer*))
  (multiple-value-bind (vars types accessors)
      (get-syms-types-places type-accessor-plist)
    `(with-accessors ,(mapcar #'quote-2 vars accessors) ,object
         (serialize* ,(apply #'append (mapcar #'quote-2 types vars))
                     :buffer ,buffer))))

(defmacro unserialize* ((&rest type-place-plist) &key (buffer '*buffer*))
  "UNSERIALIZE a list of TYPE + PLACE from the given BUFFER and execute the body.  For example:  (LET (AA BB) (UNSERIALIZE* (:UINT8 AA :INT16 BB) :BUFFER BUFFER (LIST AA BB)))"
  (multiple-value-bind (vars types places)
      (get-syms-types-places type-place-plist)
    (declare (ignore vars))
    (let ((buf (gensym "BUF-")))
      `(let* ((,buf ,buffer))
         (setf ,@(apply #'append
                        (mapcar #'(lambda (tt pp)
                                    `(,pp (unserialize ,tt :buffer ,buf)))
                                types places)))
         (values t ,buf)))))

(defmacro unserialize-slots* ((&rest type-slot-plist) object
			      &key (buffer '*buffer*))
  (multiple-value-bind (vars types slots)
      (get-syms-types-places type-slot-plist)
    (let ((obj (gensym "OBJ-")))
      `(let ((,obj ,object))
         (with-slots ,(mapcar #'quote-2 vars slots) ,obj
           (unserialize* ,(apply #'append (mapcar #'quote-2 types vars))
                         :buffer ,buffer)
           ,obj)))))

(defmacro unserialize-accessors* ((&rest type-accessor-plist) object
				  &key (buffer '*buffer*))
  (multiple-value-bind (vars types accessors)
      (get-syms-types-places type-accessor-plist)
    (let ((obj (gensym "OBJ-")))
      `(let ((,obj ,object))
         (with-accessors ,(mapcar #'quote-2 vars accessors) ,obj
           (unserialize* ,(apply #'append (mapcar #'quote-2 types vars))
                         :buffer ,buffer)
           ,obj)))))

(defmacro unserialize-let* ((&rest type-var-plist) buffer &body body)
  "UNSERIALIZE a list of TYPE + VARIABLE-NAME from the given BUFFER and execute the body.  For example:  (UNSERIALIZE-LET* (:UINT8 AA :INT16 BB) :buffer BUFFER (LIST AA BB))"
  (let ((buf (gensym "BUF-")))
    (multiple-value-bind (vars types places)
        (get-syms-types-places type-var-plist)
      (declare (ignore vars))
      `(let* ((,buf ,buffer)
              ,@(mapcar #'(lambda (pp tt)
                            `(,pp (unserialize ,tt :buffer ,buf)))
                        places types))
         (values (progn
                   ,@body)
                 ,buf)))))

(defun unserialize-list* (types &key (buffer *buffer*))
  "UNSERIALIZE a list of types from the given BUFFER into a list.  For example:  (MAPCAR #'PRINC (UNSERIALIZE-LIST* '(:UINT8 :INT16) :BUFFER  BUFFER))"
  (values (mapcar #'(lambda (tt)
                      (unserialize tt :buffer buffer))
                  types)
          buffer))

;; help define a serializer
(defmacro define-serializer ((key value buffer &key layer extra)
                             &body body)
  (let ((keysym (gensym "KEY-")))
    `(contextl:define-layered-method serialize ,@(when layer
                                                       `(:in-layer ,layer))
           ((,keysym (eql ,key)) ,value
            &key (,buffer userial::*buffer*) ,@extra &allow-other-keys)
        (declare (ignore ,keysym))
        ,@body
        ,buffer)))

;; help define an unserializer
(defmacro define-unserializer ((key buffer &key layer extra)
                               &body body)
  (multiple-value-bind (decls body)
      (userial::separate-docstring-and-decls body)
    (let ((keysym (gensym "KEY-")))
      `(contextl:define-layered-method unserialize ,@(when layer
                                                           `(:in-layer ,layer))
            ((,keysym (eql ,key))
             &key (,buffer userial::*buffer*) ,@extra &allow-other-keys)
        ,@decls
        (declare (ignore ,keysym))
        (values (progn ,@body) ,buffer)))))

;;; help build integer serialize/unserialize methods
(defmacro make-uint-serializer (key bytes &key layer)
  "Make SERIALIZE/UNSERIALIZE methods for an unsigned-int of BYTES bytes
   in length dispatched by KEY."
  `(progn
     (define-serializer (,key value buffer :layer ,layer)
       (declare (type (unsigned-byte ,(* bytes 8)) value)
                (optimize (speed 3)))
       (unroll-add-bytes buffer value ,bytes))
     (define-unserializer (,key buffer :layer ,layer)
       (declare (optimize (speed 3)))
       (unroll-get-bytes buffer ,bytes))))

;;; define standard unsigned-int serialize/deserialize methods
(make-uint-serializer :uint8  1)
(make-uint-serializer :uint16 2)
(make-uint-serializer :uint24 3)
(make-uint-serializer :uint32 4)
(make-uint-serializer :uint48 6)
(make-uint-serializer :uint64 8)

;;; progressively stored unsigned integers
(define-serializer (:uint value buffer)
  (declare (type (integer 0 *) value)
           (optimize (speed 3)))
  (labels ((store-bytes (vv)
             (declare (type (integer 0 *) vv))
             (multiple-value-bind (high low) (floor vv 128)
               (declare (type (integer 0 *) high)
                        (type (integer 0 127) low))
               (cond
                 ((zerop high) (serialize :uint8 low :buffer buffer))
                 (t            (serialize :uint8 (+ low 128) :buffer buffer)
                               (store-bytes high))))))
    (store-bytes value)))
(define-unserializer (:uint buffer)
  (declare (optimize (speed 3)))
  (labels ((fetch-bytes (&optional (vv 0) (offset 1))
             (declare (type (integer 0 *) vv)
                      (type (integer 1 *) offset))
             (unserialize-let* (:uint8 byte) buffer
               (flet ((add (bb)
                        (+ vv (* bb offset))))
                 (cond
                   ((< byte 128) (add byte))
                   (t (fetch-bytes (add (- byte 128)) (* offset 128))))))))
    (fetch-bytes)))

(defmacro make-int-serializer (key bytes &key layer)
  "Make SERIALIZE/UNSERIALIZE methods for a signed-int of BYTES bytes in length dispatched by KEY."
  (let ((vv (gensym "VV-")))
    `(progn
       (define-serializer (,key value buffer :layer ,layer)
         (declare (type (signed-byte ,(* bytes 8)) value)
                  (optimize (speed 3)))
         (let ((,vv (+ value ,(expt 2 (1- (* bytes 8))))))
           (unroll-add-bytes buffer ,vv ,bytes)))
       (define-unserializer (,key buffer :layer ,layer)
         (declare (optimize (speed 3)))
         (let ((,vv (unroll-get-bytes buffer ,bytes)))
           (declare (type (unsigned-byte ,(* bytes 8)) ,vv))
           (- ,vv ,(expt 2 (1- (* bytes 8)))))))))

;;; define standard signed-int serialize/deserialize methods
(make-int-serializer :int8  1)
(make-int-serializer :int16 2)
(make-int-serializer :int32 4)
(make-int-serializer :int64 8)

(define-serializer (:int value buffer)
  (declare (type integer value)
           (optimize (speed 3)))
  (flet ((store-and-go (vv sign)
           (multiple-value-bind (high low) (floor vv 64)
             (cond
               ((zerop high) (serialize :uint8 (+ low sign) :buffer buffer))
               (t            (serialize :uint8 (+ low sign 64) :buffer buffer)
                             (serialize :uint high :buffer buffer))))))
    (cond
      ((minusp value) (store-and-go (- value) 128))
      (t              (store-and-go value 0)))))

(define-unserializer (:int buffer)
  (declare (optimize (speed 3)))
  (flet ((get-number (vv)
           (cond
             ((<= 64 vv) (+ (* (unserialize :uint :buffer buffer) 64)
                            (- vv 64)))
             (t          vv))))
    (let ((byte (unserialize :uint8 :buffer buffer)))
      (cond
        ((<= 128 byte) (- (get-number (- byte 128))))
        (t             (get-number byte))))))

;;; floating-point type helper
(defmacro make-float-serializer (key type bytes encoder decoder
                                 &key layer)
  "Make serialize/unserialize routines for floating-point type TYPE dispatched by KEY with the given number of BYTES and an ENCODER/DECODER pair."
  (let ((vv (gensym "VV-")))
    `(progn
       (define-serializer (,key value buffer :layer ,layer)
         (declare (type ,type value)
                  (optimize (speed 3)))
         (unroll-add-bytes buffer (,encoder value) ,bytes))
       (define-unserializer (,key buffer :layer ,layer)
         (declare (optimize (speed 3)))
         (let ((,vv (unroll-get-bytes buffer ,bytes)))
           (declare (type (unsigned-byte ,(* bytes 8)) ,vv))
           (,decoder ,vv))))))

;;; floating-point type serializers/deserializers
(make-float-serializer :float32 single-float 4
		       ieee-floats:encode-float32
		       ieee-floats:decode-float32)
(make-float-serializer :float64 double-float 8
		       ieee-floats:encode-float64
		       ieee-floats:decode-float64)

;;; byte arrays without size encoded with them
(define-serializer (:raw-bytes value buffer
                    :extra ((start 0) (end (length value))))
  (declare (type (vector (unsigned-byte 8)) value))
  (assert (<= 0 start end (length value)))
  (let ((length (- end start))
        (buf-start (buffer-length :buffer buffer)))
    (buffer-advance :amount length :buffer buffer)
    (setf (subseq buffer buf-start (buffer-length :buffer buffer))
          (subseq value start end))))
(define-unserializer (:raw-bytes buffer
                      :extra (output (start 0) (end (length output))))
  (assert (<= 0 start end))
  (let* ((length (- end start))
         (buf-start (buffer-length :buffer buffer))
         (output (or output
                     (make-array (list length)
                                 :initial-element 0
                                 :element-type '(unsigned-byte 8)))))
    (declare (type (vector (unsigned-byte 8)) output))
    (assert (<= end (length output)))
    (buffer-advance :amount length :buffer buffer)
    (setf (subseq output start end)
          (subseq buffer buf-start (buffer-length :buffer buffer)))
    output))
  
;;; byte arrays with size
(define-serializer (:bytes value buffer)
  (declare (type (vector (unsigned-byte 8)) value))
  (serialize :uint (length value) :buffer buffer)
  (serialize :raw-bytes value :buffer buffer))

(define-unserializer (:bytes buffer)
  (unserialize-let* (:uint length) buffer
    (let ((value (make-array (list length)
                             :initial-element 0
                             :element-type '(unsigned-byte 8))))
      (unserialize :raw-bytes :buffer buffer :output value))))

;;; string handling
(define-serializer (:string value buffer)
  (declare (type string value))
  (serialize :bytes (trivial-utf-8:string-to-utf-8-bytes value)
	     :buffer buffer))

(define-unserializer (:string buffer)
  (let ((value (unserialize :bytes :buffer buffer)))
    (declare (type (vector (unsigned-byte 8)) value)
	     (type buffer buffer))
    (trivial-utf-8:utf-8-bytes-to-string value)))

;;; enum helper
(defmacro make-enum-serializer (type (&rest choices)
                                &key layer
                                     (minimum-bytes 0))
  "Create serialize/unserialize methods keyed by TYPE where the possible values are given by CHOICES"
  (let ((bytes (max (nth-value 0 (ceiling (log (length choices) 256)))
                    minimum-bytes)))
    `(progn
       (define-serializer (,type value buffer :layer ,layer)
         (declare (type symbol value))
	 (let ((value (ecase value
			,@(loop :for ii :from 0
			        :for vv :in choices
			        :collecting (list (if vv vv '(nil)) ii)))))
	   (declare (type (unsigned-byte ,(* bytes 8)) value))
	   (unroll-add-bytes buffer value ,bytes)))
       (define-unserializer (,type buffer :layer ,layer)
         (let ((value (unroll-get-bytes buffer ,bytes)))
           (declare (type (unsigned-byte ,(* bytes 8)) value))
	   (let ((value (ecase value
			  ,@(loop :for ii :from 0
                                  :for vv :in choices
                                  :collecting (list ii vv)))))
	     (declare (type symbol value))
             value))))))

;;; define standard enum methods
(make-enum-serializer :boolean (nil t))

(defmacro make-bitfield-serializer (type (&rest choices) &key layer)
  "Create serialize/unserialize methods keyed by TYPE where the CHOICES can either be specified singly or as a list."
  (let ((bytes (nth-value 0 (ceiling (length choices) 8))))
    `(progn
       (define-serializer (,type (value cons) buffer :layer ,layer)
         (declare (type cons value))
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
       (define-serializer (,type (value symbol) buffer :layer ,layer)
         (declare (type symbol value))
	 (let ((val (ecase value
		      ((nil) 0)
		      ,@(loop :for ii :from 0
			      :for vv :in choices
			      :collecting (list vv (expt 2 ii))))))
	   (declare (type (unsigned-byte ,(* bytes 8)) val))
	   (unroll-add-bytes buffer val ,bytes)))
       (define-unserializer (,type buffer :layer ,layer)
         (let ((value (unroll-get-bytes buffer ,bytes)))
           (declare (type (unsigned-byte ,(* bytes 8)) value))
           (assert (< value ,(expt 2 (length choices))))
           (loop :for ii :from 0 :below ,(length choices)
                 :when (plusp (logand value (expt 2 ii)))
                 :collect (svref (vector ,@choices) ii)))))))

(defmacro make-simple-serializer ((type value buffer factory &key layer extra)
                                   &rest pairs)
  `(progn
     (define-serializer (,type ,value ,buffer :layer ,layer :extra extra)
       (serialize* ,pairs :buffer ,buffer))
     (define-unserializer (,type ,buffer
                                 :layer ,layer
                                 :extra ((,value ,factory) ,@extra))
       (unserialize* ,pairs :buffer ,buffer))))

(defmacro make-slot-serializer ((type value buffer factory &key layer)
                                &rest fields)
  "Make a serialize/unserialize pair with given TYPE using the FACTORY
   form when a new instance is needed where FIELDS is a list of
   key/slot values.  For example:
      (defstruct person name (age 0))
      (make-slot-serializer (:person person buffer (make-person))
          :string name :uint8 age)"
  `(progn
     (define-serializer (,type ,value ,buffer :layer ,layer)
       (serialize-slots* ,fields ,value :buffer ,buffer))
     (define-unserializer (,type ,buffer
                                 :layer ,layer
                                 :extra ((,value ,factory)))
       (unserialize-slots* ,fields ,value :buffer ,buffer)
       ,value)))

(defmacro make-accessor-serializer ((type value buffer factory
                                     &key layer)
                                    &rest fields)
  "Make a serialize/unserialize pair with given TYPE using the FACTORY
   form when a new instance is needed where FIELDS is a list of
   key/accessor values.  For example:
      (defstruct person name (age 0))
      (make-accessor-serializer (:person person buffer (make-person))
          :string person-name :uint8 person-age)"
  `(progn
     (define-serializer (,type ,value ,buffer :layer ,layer)
       (serialize-accessors* ,fields ,value :buffer ,buffer))
     (define-unserializer (,type ,buffer
                                 :layer ,layer
                                 :extra ((,value ,factory)))
       (unserialize-accessors* ,fields ,value :buffer ,buffer)
       ,value)))

(defmacro make-list-serializer (type element-type &key layer)
  "Make a serialize/unserialize pair for the key TYPE where each element is serialized with the key ELEMENT-TYPE."
  (format t "Foo~%")
  `(progn
     (define-serializer (,type value buffer :layer ,layer)
       (declare (type list value))
       (serialize :uint32 (length value) :buffer buffer)
       (dolist (ee value)
         (serialize ,element-type ee :buffer buffer)))
     (define-unserializer (,type buffer :layer ,layer)
       (unserialize-let* (:uint32 len) buffer
         (loop :for ii :from 1 :to len
               :collecting (unserialize ,element-type :buffer buffer))))))
