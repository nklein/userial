;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :userial)

(deftype uchar   () '(unsigned-byte 8))
(deftype uint    () '(integer 0 *))
(deftype bufsize () '(integer 0 #.array-dimension-limit))
(deftype buffer ()
  "A BUFFER is an adjustable, one-dimensional array of unsigned bytes with
   a fill-pointer."
  '(array uchar (*)))

(defconstant +default-buffer-capacity+ 32768)
(defconstant +default-buffer-expand+ 8192)

;;; make-buffer function
(declaim (ftype (function (&optional bufsize) buffer)
                make-buffer))
(defun make-buffer (&optional (initial-capacity +default-buffer-capacity+))
  "Create an empty BUFFER of a given INITIAL-CAPACITY"
  (declare (type bufsize initial-capacity)
           (optimize (speed 3)))
  (make-array (list initial-capacity)
              :element-type 'uchar
              :initial-element 0
              :fill-pointer 0
              :adjustable t))

(declaim (special *buffer*)
         (type buffer *buffer*))
(defvar *buffer* (make-buffer))

(declaim (inline get-buffer)
         (ftype (function () buffer) get-buffer))
(defun get-buffer ()
  *buffer*)

(defmacro with-buffer (buffer &body body)
  "Macro to do a BODY of statements with the given BUFFER as default."
  `(let ((userial::*buffer* ,buffer))
     ,@body))

(declaim (inline buffer-length)
         (ftype (function () bufsize) buffer-length))
(defun buffer-length ()
  "Returns the current length of the BUFFER"
  (declare (optimize (speed 3)))
  (the bufsize (fill-pointer *buffer*)))

(declaim (ftype (function (bufsize) buffer) (setf buffer-length)))
(defun (setf buffer-length) (new-length)
  "Sets the length of the BUFFER to NEW-LENGTH"
  (declare (type bufsize new-length)
           (optimize (speed 3)))
  (setf (fill-pointer *buffer*) new-length)
  *buffer*)

(declaim (inline buffer-capacity)
         (ftype (function () bufsize) buffer-capacity))
(defun buffer-capacity ()
  "Returns the current capacity of the BUFFER"
  (the bufsize (array-dimension *buffer* 0)))

(declaim (ftype (function (bufsize) buffer) (setf buffer-capacity)))
(defun (setf buffer-capacity) (new-capacity)
  "Sets the capacity of the BUFFER to NEW-CAPACITY"
  (declare (type bufsize new-capacity))
  (adjust-array *buffer* (list new-capacity)
                :fill-pointer
                (min (buffer-length) new-capacity)))

(declaim (ftype (function (&optional bufsize) buffer)
                buffer-expand-if-needed))
(defun buffer-expand-if-needed (&optional (amount 1))
  "Expand the BUFFER if needed to accomodate EXPAND-BY more bytes"
  (declare (type bufsize amount)
           (optimize (speed 3)))
  (let ((capacity    (buffer-capacity))
        (needs-to-be (+ (buffer-length) amount)))
    (declare (type bufsize capacity needs-to-be))
    (when (< capacity needs-to-be)
      (setf (buffer-capacity)
            (the bufsize
              (max needs-to-be
                   (the bufsize
                     (min (* capacity 2)
                          (+ capacity +default-buffer-expand+))))))))
  *buffer*)

(declaim (inline buffer-advance)
         (ftype (function (&optional bufsize) buffer) buffer-advance))
(defun buffer-advance (&optional (amount 1))
  "Advances the BUFFER by AMOUNT bytes"
  (declare (type bufsize amount))
  (let ((new-size (+ (buffer-length) amount)))
    (declare (type bufsize new-size))
    (buffer-expand-if-needed new-size)
    (setf (fill-pointer *buffer*) new-size))
  *buffer*)

(declaim (ftype (function (uchar) buffer) buffer-add-byte))
(defun buffer-add-byte (byte)
  "Add the given BYTE to the BUFFER"
  (declare (type uchar byte)
           (optimize (speed 3)))
  (setf *buffer* (buffer-expand-if-needed 1)
        (aref *buffer* (buffer-length)) byte)
  (buffer-advance 1))

(declaim (ftype (function () (values uchar buffer)) buffer-get-byte))
(defun buffer-get-byte ()
  "Get a byte from the BUFFER"
  (declare (optimize (speed 3)))
  (values (aref *buffer* (buffer-length))
          (buffer-advance 1)))

(declaim (ftype (function () buffer) buffer-rewind))
(defun buffer-rewind ()
  "Rewind the BUFFER to the beginning"
  (declare (optimize (speed 3)))
  (setf (fill-pointer *buffer*) 0)
  *buffer*)

(defmacro unroll-add-bytes (value-form bytes)
  "Added BYTES bytes from the VALUE-FORM (most-significant byte first)"
  (let ((value-sym (gensym "VALUE-")))
    `(let ((,value-sym (ldb (byte ,(* bytes 8) 0) ,value-form)))
       (declare (type uint ,value-sym)
                (optimize (speed 3)))
       (progn ,@(loop :for ii :from (1- bytes) :downto 0
                    :collecting `(buffer-add-byte
                                  (ldb (byte 8 ,(* 8 ii)) ,value-sym)))
              *buffer*))))

(defmacro unroll-get-bytes (bytes)
  "Get BYTES bytes (most-significant byte first)"
  (let ((value-sym (gensym "VALUE-")))
    `(let ((,value-sym 0))
       (declare (type uint ,value-sym)
                (optimize (speed 3)))
       (progn
         ,@(loop :for ii :from (1- bytes) :downto 0
                 :collecting `(setf (ldb (byte 8 ,(* 8 ii)) ,value-sym)
                                    (buffer-get-byte)))
         (values ,value-sym *buffer*)))))
