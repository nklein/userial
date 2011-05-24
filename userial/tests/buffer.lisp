;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :userial-tests)

;;; prepare some buffers of different sizes
(nst:def-fixtures simple-buffers
    (:documentation "Prepare some buffers of different sizes")
  (small  (make-buffer 10))
  (medium (make-buffer 1024))
  (large  (make-buffer))
  (base-buffers (list small medium large)))

;;; prepare a criterion that looks for an array whose zeroth dimension is NN
(nst:def-criterion-alias (:array-dim-0 nn)
    `(:apply (lambda (a) (array-dimension a 0)) (:eql ,nn)))

;;; test to make sure buffers are constructed as expected
(nst:def-test-group buffer-construction (simple-buffers)
  (:documentation "Test construction of buffers")
  (nst:def-test make-buffer (:each (:true)) base-buffers)
  (nst:def-test sizes-right (:seq (:array-dim-0 10)
				  (:array-dim-0 1024)
				  (:array-dim-0 32768))
    base-buffers)
  (nst:def-test fill-pointer-is-right (:each (:apply fill-pointer (:eql 0)))
    base-buffers)
  (nst:def-test element-type-is-right
      (:each (:apply array-element-type
		     (:equalp '(unsigned-byte 8)))) base-buffers))

;;; prepare a criterion that checks the length of a given array
(nst:def-criterion-alias (:length-is nn)
    `(:apply (lambda (b) (with-buffer b (buffer-length))) (:eql ,nn)))

;;; prepare a criterion that checks the length of a given array
(nst:def-criterion-alias (:capacity-is nn)
    `(:apply (lambda (b) (with-buffer b (buffer-capacity))) (:eql ,nn)))

(nst:def-test-group buffer-information (simple-buffers)
  (:documentation "Test querying of buffer properties")
  (nst:def-test length-right (:seq (:length-is 0)
				   (:length-is 0)
				   (:length-is 0))
    base-buffers)
  (nst:def-test capacity-right (:seq (:capacity-is 10)
				     (:capacity-is 1024)
				     (:capacity-is 32768))
    base-buffers)
  (nst:def-test advance-works (:seq (:length-is 512)
				    (:length-is 512)
				    (:length-is 512))
    (dolist (buf base-buffers base-buffers)
      (with-buffer buf
        (buffer-advance 512))))
  (nst:def-test rewind-works (:seq (:length-is 0)
				   (:length-is 0)
				   (:length-is 0))
    (mapcar #'(lambda (b) (with-buffer b (buffer-rewind))) base-buffers))
  (nst:def-test with-buffer-works (:seq (:eql 0)
					(:eql 10)
					(:eql 32))
    (with-buffer (make-buffer 10)
      (let* ((a (nth-value 0 (buffer-length)))
	     (b (nth-value 0 (buffer-capacity))))
	(buffer-advance 32)
	(list a b (nth-value 0 (buffer-length)))))))
		    

	  
