;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :userial)

(defmacro symbol-cond ((condition &rest args) &rest rest)
  (destructuring-bind (package symbol) condition
    (let ((pkg (find-package package)))
      (if pkg
	  (let ((sym (find-symbol (symbol-name symbol) pkg)))
	    (if sym
		`(,sym ,@args)
		(when rest
		  `(symbol-cond ,@rest))))
	  (when rest
	    `(symbol-cond ,@rest))))))

(defmacro serialize-log (category &rest serialize-args)
  (let ((serialized-message `(serialize* ,serialize-args
					:buffer (make-buffer))))
    `(symbol-cond
      ((:cl-log log-message) ,category ,serialized-message))))
