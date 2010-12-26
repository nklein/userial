
(in-package :userial-tests)

(nst:def-fixtures simple-buffers ()
  (small  (make-buffer 10))
  (medium (make-buffer 1024))
  (large  (make-buffer))
  (base-buffers (list small medium large)))

(nst:def-criterion-alias (:array-dim-0 nn)
    `(:apply (lambda (a) (array-dimension a 0)) (:eql ,nn)))

(nst:def-test-group buffer-construction (simple-buffers)
  (nst:def-test make-buffer (:each (:true)) base-buffers)
  (nst:def-test sizes-right (:seq (:array-dim-0 10)
				  (:array-dim-0 1024)
				  (:array-dim-0 32768))
    base-buffers)
  (nst:def-test fill-pointer-is-right (:each (:apply fill-pointer (:eql 0)))
    base-buffers))

(nst:def-fixtures displaced-buffers (:uses simple-buffers)
  (small-0 (make-displaced-buffer small))
  (small-5 (make-displaced-buffer small 5))
  (small-displaced (list small-0 small-5))
  (rewound-smalls (mapcar #'rewind-buffer small-displaced)))

(nst:def-criterion-alias (:displacement-is array offset)
    `(:apply (lambda (a) (multiple-value-list (array-displacement a)))
	     (:seq (:eq ,array) (:eql ,offset))))

(nst:def-criterion-alias (:fill-pointer-is nn)
    `(:apply fill-pointer (:eql ,nn)))

(nst:def-test-group displaced-buffer-construction (simple-buffers
						   displaced-buffers)
  (nst:def-test displaced-to-small (:each (:displacement-is small 0))
    small-displaced)
  (nst:def-test fill-pointer-is-right (:seq (:fill-pointer-is 0)
					    (:fill-pointer-is 5))
    small-displaced))

(nst:def-test-group rewound-buffer-tests (simple-buffers displaced-buffers)
  (nst:def-test displaced-to-small (:seq (:displacement-is small-0 0)
					 (:displacement-is small-5 0))
    rewound-smalls)
  (nst:def-test fill-pointer-is-right (:each (:fill-pointer-is 0))
    rewound-smalls))
