(in-package :userial)

(defun retrieve-until (until llist result)
  (cond
    ((or (null llist)
         (member (first llist) until))  (nreverse result))
    (t (retrieve-until until
                       (rest llist)
                       (cons (first llist) result)))))

(defun retrieve-paired-until (until llist result)
  (cond
    ((or (null llist)
         (member (first llist) until))  (nreverse result))
    ((member (first llist) '(&optional &key &allow-other-keys))
     (retrieve-paired-until until
                            (rest llist)
                            (cons (first llist) result)))
    ((= (length llist) 1)
        (error "Odd number of key-variable pairs at ~S" llist))
    (t (retrieve-paired-until until
                              (rest (rest llist))
                              (cons (list (second llist)
                                          (first llist)
                                          (second llist))
                                    result)))))

(defun retrieve-from-until (start until llist)
  (if (member start '(&optional &key))
      (retrieve-paired-until until (member start llist) nil)
      (retrieve-until until (member start llist) nil)))

(defun pluck-apart-keyed-lambda-list (llist)
  (let* ((required (retrieve-paired-until '(&optional &rest &key) llist nil))
         (optional (retrieve-from-until '&optional '(&rest &key) llist))
         (rest     (retrieve-from-until '&rest '(&key) llist))
         (key      (retrieve-from-until '&key '(&allow-other-keys &aux) llist))
         (allow    (retrieve-from-until '&allow-other-keys '(&aux) llist))
         (aux      (retrieve-from-until '&aux '() llist)))
    (values required optional rest key allow aux)))

(defun sym-for-opt (opt-cons)
  (let ((opt (first opt-cons)))
    (if (listp opt)
        (first opt)
        opt)))

(defun sym-for-key (key-cons)
  (let ((key (first key-cons)))
    (if (listp key)
        (if (listp (first key))
            (second (first key))
            (first key))
        key)))

(defun key-for-key (key-cons)
  (let ((key (first key-cons)))
    (if (listp key)
        (if (listp (first key))
            (first (first key))
            (intern (symbol-name (first key)) :keyword))
        (intern (symbol-name key) :keyword))))

(defun it-or-third (item)
  (if (listp item)
      (third item)
      item))

(defun serialize-req (item)
  `(serialize ,(second item) ,(car item)))

(defun serialize-opt (item)
  `(when ,(third (first item))
     (serialize ,(second item) ,(first (first item)))))

(defun serialize-key (item)
  `(when ,(third (first item))
     (serialize ,(second item)
                ,(if (listp (first (first item)))
                     (second (first (first item)))
                     (first (first item))))))

(defun argify (item)
  (if (symbolp item)
      item
      (let ((argp (gensym "ARG-P-")))
        (unless (listp (car item))
          (setf (car item) (list (car item) "UNSPECIFIED" argp)))
        (case (length (car item))
          (1 (setf (car item)
                   (append (first item) (list "UNSPECIFIED" argp))))
          (2 (setf (first item)
                   (append (first item) (list argp)))))
        (first item))))

(defun got-arg (item)
  `(when ,(third (first item))
     '(,(sym-for-key item))))

(defmacro define-serializing-funcall ((buffer-func func &key layer)
                                        (&rest keyed-lambda-list)
                                      &body body)
  (let ((args (gensym "ARGS-"))
        (which (gensym "WHICH-"))
        (more-keys (gensym "MORE-KEYS-")))
    (labels ((unserialize-req (item)
               `(push (unserialize ,(second item)) ,args))

             (unserialize-opt (item)
               `(when (member ',(sym-for-opt item) ,which)
                  (push (unserialize ,(second item)) ,args)))

             (unserialize-key (item)
               `(when (member ',(sym-for-key item) ,which)
                  (push ,(key-for-key item) ,args)
                  (push (unserialize ,(second item)) ,args))))
      
      (multiple-value-bind (req opt rst key allow aux)
          (pluck-apart-keyed-lambda-list keyed-lambda-list)
        
        `(progn
           ,(when (or (rest opt) (rest key))
                  `(make-bitfield-serializer
                      ',buffer-func
                      (,@(mapcar #'sym-for-opt (rest opt))
                       ,@(mapcar #'sym-for-key (rest key)))
                      :layer ,layer))
           
           (defun ,buffer-func (,@(mapcar #'car req)
                                ,@(mapcar #'argify opt)
                                ,@(mapcar #'argify key))
             ,@(when (or (rest opt) (rest key))
                  `((serialize ',buffer-func
                               (funcall #'append
                                        ,@(mapcar #'got-arg (rest opt))
                                        ,@(mapcar #'got-arg (rest key))))))
             ,@(mapcar #'serialize-req req)
             ,@(mapcar #'serialize-opt (rest opt))
             ,@(mapcar #'serialize-key (rest key))
             *buffer*)
           
           (defun ,func (&rest ,more-keys &key &allow-other-keys)
             (let (,@(when (or (rest opt) (rest key))
                        `((,which (unserialize ',buffer-func))))
                   ,args)
               ,@(mapcar #'unserialize-req req)
               ,@(mapcar #'unserialize-opt (rest opt))
               ,@(mapcar #'unserialize-key (rest key))
               
               (values 
                (apply #'(lambda (,@(mapcar #'it-or-third req)
                                  ,@(mapcar #'it-or-third opt)
                                  ,@rst
                                  ,@(mapcar #'it-or-third key)
                                  ,@allow
                                  ,@aux)
                           ,@body)
                       (append (nreverse ,args) ,more-keys))
                *buffer*))))))))

#|
(define-serializing-funcall (buffer-myfunc myfunc :layer nil)
       (:uint8 a
        &optional :string (b nil)
        &rest c
        &key :uint d :uint (e 0 e-p)
        :string ((:ff f)) &allow-other-keys
        &aux (g (+ a 6)))
  ;;; body goes here
  (list a b c d e e-p f g))

(define-serializing-funcall (mylogin-ser mylogin-uns)
       (:string username :string passwd &optional :string (nickname username))
  (format nil "LOGIN: USER = ~S, PASSWD = ~S, NICKNAME = ~S"
               username passwd nickname))
|#