;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :userial)

;;; serialize generic function
(contextl:define-layered-function serialize (type value &key &allow-other-keys)
  (:documentation "Method used to serialize a VALUE of given TYPE into BUFFER")
  (:method (type value &key &allow-other-keys)
    "There is no default way to serialize something, so chuck an error."
    (declare (ignore value))
    (error "SERIALIZE not supported for type ~A" type)))

;;; unserialize generic function
(contextl:define-layered-function unserialize (type &key &allow-other-keys)
  (:documentation
     "Method used to unserialize a value of given TYPE from BUFFER")
  (:method (type &key &allow-other-keys)
    "There is no default way to unserialize something, so chuck an error."
    (error "UNSERIALIZE not supported for type ~A" type)))

(defmacro serialize* (&rest type-value-pairs)
  "SERIALIZE a list of TYPE + VALUE into BUFFER returning the final BUFFER.  For example:  (SERIALIZE* :UINT8 5 :INT16 -10)"
  (multiple-value-bind (vars types values)
      (get-syms-types-places type-value-pairs)
    (declare (ignore vars))
    `(progn
       ,@(mapcar #'(lambda (tt vv)
                     `(serialize ,tt ,vv))
                 types values))))

(defmacro serialize-slots* (object &rest type-slot-plist)
  (multiple-value-bind (vars types slots)
      (get-syms-types-places type-slot-plist)
    `(with-slots ,(mapcar #'quote-2 vars slots) ,object
         (serialize* ,@(apply #'append (mapcar #'quote-2 types vars))))))

(defmacro serialize-accessors* (object &rest type-accessor-plist)
  (multiple-value-bind (vars types accessors)
      (get-syms-types-places type-accessor-plist)
    `(with-accessors ,(mapcar #'quote-2 vars accessors) ,object
         (serialize* ,@(apply #'append (mapcar #'quote-2 types vars))))))

(defmacro unserialize* (&rest type-place-plist)
  "UNSERIALIZE a list of TYPE + PLACE from the given BUFFER and execute the body.  For example:  (LET (AA BB) (UNSERIALIZE* :UINT8 AA :INT16 BB) (LIST AA BB))"
  (multiple-value-bind (vars types places)
      (get-syms-types-places type-place-plist)
    (declare (ignore vars))
    `(setf ,@(apply #'append
                    (mapcar #'(lambda (tt pp)
                                `(,pp (unserialize ,tt)))
                            types places)))))

(defmacro unserialize-slots* (object &rest type-slot-plist)
  (multiple-value-bind (vars types slots)
      (get-syms-types-places type-slot-plist)
    (let ((obj (gensym "OBJ-")))
      `(let ((,obj ,object))
         (with-slots ,(mapcar #'quote-2 vars slots) ,obj
           (unserialize* ,@(apply #'append (mapcar #'quote-2 types vars))))
         ,obj))))

(defmacro unserialize-accessors* (object &rest type-accessor-plist)
  (multiple-value-bind (vars types accessors)
      (get-syms-types-places type-accessor-plist)
    (let ((obj (gensym "OBJ-")))
      `(let ((,obj ,object))
         (with-accessors ,(mapcar #'quote-2 vars accessors) ,obj
           (unserialize* ,@(apply #'append (mapcar #'quote-2 types vars)))
           ,obj)))))

(defmacro unserialize-let* ((&rest type-var-plist) &body body)
  "UNSERIALIZE a list of TYPE + VARIABLE-NAME from the given BUFFER and execute the body.  For example:  (UNSERIALIZE-LET* (:UINT8 AA :INT16 BB) (LIST AA BB))"
  (multiple-value-bind (vars types places)
      (get-syms-types-places type-var-plist)
    (declare (ignore vars))
    `(let* (,@(mapcar #'(lambda (pp tt)
                          `(,pp (unserialize ,tt)))
                      places types))
       (values (progn ,@body) *buffer*))))

(defun unserialize-list* (types)
  "UNSERIALIZE a list of types from the given BUFFER into a list.  For example:  (MAPCAR #'PRINC (UNSERIALIZE-LIST* '(:UINT8 :INT16) :BUFFER  BUFFER))"
  (values (mapcar #'(lambda (tt)
                      (unserialize tt))
                  types)
          *buffer*))

;; help define a serializer
(defmacro define-serializer ((key value &key layer extra)
                             &body body)
  (let ((keysym (gensym "KEY-")))
    `(contextl:define-layered-method serialize ,@(when layer
                                                       `(:in-layer ,layer))
           ((,keysym (eql ,key)) ,value
            &key ,@extra &allow-other-keys)
        (declare (ignore ,keysym))
        ,@body
        *buffer*)))

;; help define an unserializer
(defmacro define-unserializer ((key &key layer extra)
                               &body body)
  (multiple-value-bind (decls body)
      (userial::separate-docstring-and-decls body)
    (let ((keysym (gensym "KEY-")))
      `(contextl:define-layered-method unserialize ,@(when layer
                                                           `(:in-layer ,layer))
            ((,keysym (eql ,key))
             &key ,@extra &allow-other-keys)
        ,@decls
        (declare (ignore ,keysym))
        (values (progn ,@body) *buffer*)))))

;;; help build integer serialize/unserialize methods
(defmacro make-uint-serializer (key bytes &key layer)
  "Make SERIALIZE/UNSERIALIZE methods for an unsigned-int of BYTES bytes
   in length dispatched by KEY."
  `(progn
     (define-serializer (,key value :layer ,layer)
       (declare (type (unsigned-byte ,(* bytes 8)) value)
                (optimize (speed 3)))
       (unroll-add-bytes value ,bytes))
     (define-unserializer (,key :layer ,layer)
       (declare (optimize (speed 3)))
       (unroll-get-bytes ,bytes))))

;;; define standard unsigned-int serialize/deserialize methods
(make-uint-serializer :uint8  1)
(make-uint-serializer :uint16 2)
(make-uint-serializer :uint24 3)
(make-uint-serializer :uint32 4)
(make-uint-serializer :uint48 6)
(make-uint-serializer :uint64 8)

;;; progressively stored unsigned integers
(define-serializer (:uint value)
  (declare (type (integer 0 *) value)
           (optimize (speed 3)))
  (labels ((store-bytes (vv)
             (declare (type (integer 0 *) vv))
             (multiple-value-bind (high low) (floor vv 128)
               (declare (type (integer 0 *) high)
                        (type (integer 0 127) low))
               (cond
                 ((zerop high) (serialize :uint8 low))
                 (t            (serialize :uint8 (+ low 128))
                               (store-bytes high))))))
    (store-bytes value)))
(define-unserializer (:uint)
  (declare (optimize (speed 3)))
  (labels ((fetch-bytes (&optional (vv 0) (offset 1))
             (declare (type (integer 0 *) vv)
                      (type (integer 1 *) offset))
             (unserialize-let* (:uint8 byte)
               (flet ((add (bb)
                        (+ vv (* bb offset))))
                 (cond
                   ((< byte 128) (add byte))
                   (t (fetch-bytes (add (- byte 128)) (* offset 128))))))))
    (fetch-bytes)))

(defmacro twos-complement (val bytes)
  `(1+ (logxor ,val ,(1- (expt 2 (* bytes 8))))))

(defmacro make-int-serializer (key bytes &key layer)
  "Make SERIALIZE/UNSERIALIZE methods for a signed-int of BYTES bytes in length dispatched by KEY."
  (let ((vv (gensym "VV-")))
    `(progn
       (define-serializer (,key value :layer ,layer)
         (declare (type (signed-byte ,(* bytes 8)) value)
                  (optimize (speed 3)))
         (let ((,vv (if (minusp value)
                        (twos-complement (- value) ,bytes)
                        value)))
           (unroll-add-bytes ,vv ,bytes)))
       (define-unserializer (,key :layer ,layer)
         (declare (optimize (speed 3)))
         (let ((,vv (unroll-get-bytes ,bytes)))
           (declare (type (unsigned-byte ,(* bytes 8)) ,vv))
           (if (logbitp ,(1- (* bytes 8)) ,vv)
               (- (twos-complement ,vv ,bytes))
               ,vv))))))

;;; define standard signed-int serialize/deserialize methods
(make-int-serializer :int8  1)
(make-int-serializer :int16 2)
(make-int-serializer :int32 4)
(make-int-serializer :int64 8)

(define-serializer (:int value)
  (declare (type integer value)
           (optimize (speed 3)))
  (flet ((store-and-go (vv sign)
           (multiple-value-bind (high low) (floor vv 64)
             (cond
               ((zerop high) (serialize :uint8 (+ low sign)))
               (t            (serialize :uint8 (+ low sign 64))
                             (serialize :uint high))))))
    (cond
      ((minusp value) (store-and-go (- value) 128))
      (t              (store-and-go value 0)))))

(define-unserializer (:int)
  (declare (optimize (speed 3)))
  (flet ((get-number (vv)
           (cond
             ((<= 64 vv) (+ (* (unserialize :uint) 64)
                            (- vv 64)))
             (t          vv))))
    (declare (ftype (function (uint) uint) get-number))
    (let ((byte (unserialize :uint8)))
      (declare (type uchar byte))
      (cond
        ((<= 128 byte) (- (get-number (- byte 128))))
        (t             (get-number byte))))))

;;; floating-point type helper
(defmacro make-float-serializer (key type bytes encoder decoder
                                 &key layer)
  "Make serialize/unserialize routines for floating-point type TYPE dispatched by KEY with the given number of BYTES and an ENCODER/DECODER pair."
  (let ((vv (gensym "VV-")))
    `(progn
       (define-serializer (,key value :layer ,layer)
         (declare (type ,type value)
                  (optimize (speed 3)))
         (unroll-add-bytes (,encoder value) ,bytes))
       (define-unserializer (,key :layer ,layer)
         (declare (optimize (speed 3)))
         (let ((,vv (unroll-get-bytes ,bytes)))
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
(define-serializer (:raw-bytes value
                    :extra ((start 0) (end (length value))))
  (declare (type (vector (unsigned-byte 8)) value))
  (assert (<= 0 start end (length value)))
  (let ((length (- end start))
        (buf-start (buffer-length)))
    (buffer-advance length)
    (setf (subseq *buffer* buf-start (buffer-length))
          (subseq value start end))))

(define-unserializer (:raw-bytes
                      :extra (output (start 0) (end (length output))))
  (assert (<= 0 start end))
  (let* ((length (- end start))
         (buf-start (buffer-length))
         (output (or output
                     (make-array (list length)
                                 :initial-element 0
                                 :element-type '(unsigned-byte 8)))))
    (declare (type (vector (unsigned-byte 8)) output))
    (assert (<= end (length output)))
    (buffer-advance length)
    (setf (subseq output start end)
          (subseq *buffer* buf-start (buffer-length)))
    output))
  
;;; byte arrays with size
(define-serializer (:bytes value)
  (declare (type (vector (unsigned-byte 8)) value))
  (serialize :uint (length value))
  (serialize :raw-bytes value))

(define-unserializer (:bytes)
  (unserialize-let* (:uint length)
    (let ((value (make-array (list length)
                             :initial-element 0
                             :element-type '(unsigned-byte 8))))
      (unserialize :raw-bytes :output value))))

;;; string handling
(define-serializer (:string value)
  (declare (type string value))
  (serialize :bytes (trivial-utf-8:string-to-utf-8-bytes value)))

(define-unserializer (:string)
  (let ((value (unserialize :bytes)))
    (declare (type (vector (unsigned-byte 8)) value))
    (trivial-utf-8:utf-8-bytes-to-string value)))

;;; enum helper
(defmacro make-enum-serializer (type (&rest choices)
                                &key layer
                                     (minimum-bytes 0))
  "Create serialize/unserialize methods keyed by TYPE where the possible values are given by CHOICES"
  (let ((bytes (max (nth-value 0 (ceiling (log (length choices) 256)))
                    minimum-bytes)))
    `(progn
       (define-serializer (,type value :layer ,layer)
         (declare (type symbol value))
         (let ((value (ecase value
                        ,@(loop :for ii :from 0
                                :for vv :in choices
                                :collecting (list (if vv vv '(nil)) ii)))))
           (declare (type (unsigned-byte ,(* bytes 8)) value))
           (unroll-add-bytes value ,bytes)))
       (define-unserializer (,type :layer ,layer)
         (let ((value (unroll-get-bytes ,bytes)))
           (declare (type (unsigned-byte ,(* bytes 8)) value))
           (let ((value (ecase value
                          ,@(loop :for ii :from 0
                                  :for vv :in choices
                                  :collecting (list ii `',vv)))))
             (declare (type symbol value))
             value))))))

;;; define standard enum methods
(make-enum-serializer :boolean (nil t))

(defmacro make-bitfield-serializer (type (&rest choices) &key layer)
  "Create serialize/unserialize methods keyed by TYPE where the CHOICES can either be specified singly or as a list."
  (let ((bytes (nth-value 0 (ceiling (length choices) 8))))
    `(progn
       (define-serializer (,type (value cons) :layer ,layer)
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
           (unroll-add-bytes val ,bytes)))
       (define-serializer (,type (value symbol) :layer ,layer)
         (declare (type symbol value))
         (let ((val (ecase value
                      ((nil) 0)
                      ,@(loop :for ii :from 0
                              :for vv :in choices
                              :collecting (list vv (expt 2 ii))))))
           (declare (type (unsigned-byte ,(* bytes 8)) val))
           (unroll-add-bytes val ,bytes)))
       (define-unserializer (,type :layer ,layer)
         (let ((value (unroll-get-bytes ,bytes)))
           (declare (type (unsigned-byte ,(* bytes 8)) value))
           (assert (< value ,(expt 2 (length choices))))
           (loop :for ii :from 0 :below ,(length choices)
                 :when (plusp (logand value (expt 2 ii)))
                 :collect (svref (vector ,@(mapcar #'(lambda (cc)
                                                       `',cc)
                                                   choices))
                                 ii)))))))

(defmacro make-simple-serializer ((type value factory &key layer extra)
                                   &rest pairs)
  `(progn
     (define-serializer (,type ,value :layer ,layer :extra extra)
       (serialize* ,@pairs))
     (define-unserializer (,type :layer ,layer
                                 :extra ((,value ,factory) ,@extra))
       (unserialize* ,@pairs))))

(defmacro make-slot-serializer ((type value factory &key layer)
                                &rest fields)
  "Make a serialize/unserialize pair with given TYPE using the FACTORY
   form when a new instance is needed where FIELDS is a list of
   key/slot values.  For example:
      (defstruct person name (age 0))
      (make-slot-serializer (:person person buffer (make-person))
          :string name :uint8 age)"
  `(progn
     (define-serializer (,type ,value :layer ,layer)
       (serialize-slots* ,value ,@fields))
     (define-unserializer (,type :layer ,layer
                                 :extra ((,value ,factory)))
       (unserialize-slots* ,value ,@fields)
       ,value)))

(defmacro make-accessor-serializer ((type value factory
                                     &key layer)
                                    &rest fields)
  "Make a serialize/unserialize pair with given TYPE using the FACTORY
   form when a new instance is needed where FIELDS is a list of
   key/accessor values.  For example:
      (defstruct person name (age 0))
      (make-accessor-serializer (:person person buffer (make-person))
          :string person-name :uint8 person-age)"
  `(progn
     (define-serializer (,type ,value :layer ,layer)
       (serialize-accessors* ,value ,@fields))
     (define-unserializer (,type :layer ,layer
                                 :extra ((,value ,factory)))
       (unserialize-accessors* ,value ,@fields)
       ,value)))

;;; maker to create serializers for lists
(defmacro make-list-serializer (type element-type &key layer)
  "Make a serialize/unserialize pair for the key TYPE where each element is serialized with the key ELEMENT-TYPE."
  `(progn
     (define-serializer (,type value :layer ,layer)
       (declare (type list value))
       (serialize :uint32 (length value))
       (dolist (ee value)
         (serialize ,element-type ee)))
     (define-unserializer (,type :layer ,layer)
       (unserialize-let* (:uint32 len)
         (loop :for ii :from 1 :to len
               :collecting (unserialize ,element-type))))))

;;; maker to create serializers for fixed length vectors
(defmacro make-vector-serializer (type element-type length &key layer)
  (let ((elt (gensym "ELT-"))
        (value (gensym "VALUE-")))
    `(progn
       (define-serializer (,type ,value :layer ,layer)
         (declare (type (vector * ,length) value))
         (loop :for ,elt :across ,value
            :do (serialize ,element-type ,elt)))
       (define-unserializer (,type :layer ,layer)
         (vector ,@(loop :for elt :from 0 :below length
                      :collecting `(unserialize ,element-type)))))))

;;; serializer for keywords
(define-serializer (:keyword value)
  (declare (type symbol value))
  (serialize :string (symbol-name value)))

(define-unserializer (:keyword)
  (unserialize-let* (:string name)
    (intern name :keyword)))

;;; serializer for symbols
(define-serializer (:symbol value)
  (declare (type symbol value))
  (serialize* :string (symbol-name value)
              :string (package-name (symbol-package value))))

(define-unserializer (:symbol)
  (unserialize-let* (:string name :string package)
    (intern name package)))

;;; maker for aliasing serializers
(defmacro make-alias-serializer (is-key was-key &key layer)
  (let ((keysym (gensym "KEY-"))
        (value (gensym "VAL-"))
        (rest (gensym "REST-")))
    `(progn
       (contextl:define-layered-method serialize ,@(when layer
                                                         `(:in-layer ,layer))
             ((,keysym (eql ,is-key)) ,value &rest ,rest
                                             &key &allow-other-keys)
         (declare (ignore ,keysym))
         (apply #'serialize ,was-key ,value ,rest))
       (contextl:define-layered-method unserialize ,@(when layer
                                                           `(:in-layer ,layer))
             ((,keysym (eql ,is-key)) &rest ,rest &key &allow-other-keys)
         (declare (ignore ,keysym))
         (apply #'unserialize ,was-key ,rest)))))

;;; serializer for objects fetched by a key
(defmacro make-key-*-serializer (serialize-it unserialize-it
                                 (key var
                                         type-getter-var-list
                                         finder-form
                                         &key layer extra)
                                    &rest type-*-pairs)
  `(progn
     (define-serializer (,key ,var :layer ,layer :extra ,extra)
       (serialize* ,@(loop :for aa :on type-getter-var-list :by #'cdddr
                        :appending (list (first aa)
                                         (let ((ff (second aa)))
                                           (if (symbolp ff)
                                               `(slot-value ,var ',ff)
                                               ff)))))
       (,serialize-it ,var ,@type-*-pairs))
     (define-unserializer (,key :layer ,layer
                                :extra (,var ,@extra))
       (declare (ignorable ,var))
       (unserialize-let* ,(loop :for aa :on type-getter-var-list :by #'cdddr
                             :appending (list (first aa) (third aa)))
         (,unserialize-it ,finder-form ,@type-*-pairs)))))

(defmacro make-key-slot-serializer ((key var
                                         type-getter-var-list
                                         finder-form
                                         &key layer extra)
                                    &rest type-slot-pairs)
  `(make-key-*-serializer serialize-slots* unserialize-slots*
                          (,key ,var ,type-getter-var-list ,finder-form
                                :layer ,layer :extra ,extra)
                          ,@type-slot-pairs))

(defmacro make-key-accessor-serializer ((key var
                                             type-getter-var-list
                                             finder-form
                                             &key layer extra)
                                        &rest type-accessor-pairs)
  `(make-key-*-serializer serialize-accessors* unserialize-accessors*
                          (,key ,var ,type-getter-var-list ,finder-form
                                :layer ,layer :extra ,extra)
                          ,@type-accessor-pairs))

(defmacro make-global-variable-serializer
    ((key value-key &key layer) &rest global-vars)
  (let ((var-sym (gensym "VAR-"))
        (sym (gensym "SYM-")))
    `(make-key-accessor-serializer
         (,key ,var-sym (:symbol (ecase ,var-sym (,global-vars ,var-sym)) ,sym)
                        (ecase ,sym (,global-vars ,sym))
                        :layer ,layer)
         ,value-key symbol-value)))
