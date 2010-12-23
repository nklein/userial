
(in-package :unet-tests)

(nst:def-fixtures simple-packets ()
  (small  (make-packet 10))
  (medium (make-packet 1024))
  (large  (make-packet))
  (base-packets (list small medium large)))

(nst:def-criterion-alias (:array-dim-0 nn)
    `(:apply (lambda (a) (array-dimension a 0)) (:eql ,nn)))

(nst:def-test-group packet-construction (simple-packets)
  (nst:def-test make-packet (:each (:true)) base-packets)
  (nst:def-test sizes-right (:seq (:array-dim-0 10)
				  (:array-dim-0 1024)
				  (:array-dim-0 32768))
    base-packets)
  (nst:def-test fill-pointer-is-right (:each (:apply fill-pointer (:eql 0)))
    base-packets))

(nst:def-fixtures displaced-packets (:uses simple-packets)
  (small-0 (make-displaced-packet small))
  (small-5 (make-displaced-packet small 5))
  (small-displaced (list small-0 small-5))
  (rewound-smalls (mapcar #'rewind-packet small-displaced)))

(nst:def-criterion-alias (:displacement-is array offset)
    `(:apply (lambda (a) (multiple-value-list (array-displacement a)))
	     (:seq (:eq ,array) (:eql ,offset))))

(nst:def-criterion-alias (:fill-pointer-is nn)
    `(:apply fill-pointer (:eql ,nn)))

(nst:def-test-group displaced-packet-construction (simple-packets
						   displaced-packets)
  (nst:def-test displaced-to-small (:each (:displacement-is small 0))
    small-displaced)
  (nst:def-test fill-pointer-is-right (:seq (:fill-pointer-is 0)
					    (:fill-pointer-is 5))
    small-displaced))

(nst:def-test-group rewound-packet-tests (simple-packets displaced-packets)
  (nst:def-test displaced-to-small (:seq (:displacement-is small-0 0)
					 (:displacement-is small-5 0))
    rewound-smalls)
  (nst:def-test fill-pointer-is-right (:each (:fill-pointer-is 0))
    rewound-smalls))
