
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

;;; prepare some buffers displaced into other buffers
(nst:def-fixtures displaced-buffers
    (:uses simple-buffers
     :documentation "Prepare some buffers displaced into other buffers")
  (small-0 (make-displaced-buffer small))
  (small-5 (make-displaced-buffer small 5))
  (small-displaced (list small-0 small-5))
  (rewound-smalls (mapcar #'rewind-buffer small-displaced)))

;;; prepare a criterion that checks the displacement of a given array
(nst:def-criterion-alias (:displacement-is array offset)
    `(:apply (lambda (a) (multiple-value-list (array-displacement a)))
	     (:seq (:eq ,array) (:eql ,offset))))

;;; prepare a criterion that checks the fill-pointer of a given array
(nst:def-criterion-alias (:fill-pointer-is nn)
    `(:apply fill-pointer (:eql ,nn)))

;;; tests of buffers displaced into other buffers
(nst:def-test-group displaced-buffer-construction (simple-buffers
						   displaced-buffers)
  (:documentation "Test construction of buffers displaced into other buffers")
  (nst:def-test displaced-to-small (:each (:displacement-is small 0))
    small-displaced)
  (nst:def-test fill-pointer-is-right (:seq (:fill-pointer-is 0)
					    (:fill-pointer-is 5))
    small-displaced))

;;; tests of buffers that have been rewound
(nst:def-test-group rewound-buffer-tests (simple-buffers displaced-buffers)
  (:documentation "Test results of rewinding buffers")
  (nst:def-test displaced-to-small (:seq (:displacement-is small-0 0)
					 (:displacement-is small-5 0))
    rewound-smalls)
  (nst:def-test fill-pointer-is-right (:each (:fill-pointer-is 0))
    rewound-smalls))
