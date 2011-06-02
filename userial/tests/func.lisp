;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :userial-tests)

;;; prepare a function to use in tests
(define-serializing-funcall (call-to-buffer call-from-buffer)
    (:uint8 a
       &optional :string b
       &rest c
       &key :string d :string (e "e") :uint8 ((:ff f) 1 f-p)
       &aux (g (+ a f)))
  (list a b c d e f f-p g))

(nst:def-test-group test-serializing-funcall ()
  (:documentation "Define serializing funcall")
  (nst:def-test serializing-funcall-req (:array-equalp (0 65))
    (with-buffer (make-buffer 2)
      (call-to-buffer 65)))
  
  (nst:def-test serializing-funcall-req+opt (:array-equalp (1 65 1 66))
    (with-buffer (make-buffer 4)
      (call-to-buffer 65 "B")))
  
  (nst:def-test serializing-funcall-req+opt+key
      (:array-equalp (5 65 1 66 1 69))
    (with-buffer (make-buffer 6)
      (call-to-buffer 65 "B" :e "E")))
  
  (nst:def-test serializing-funcall-all
      (:array-equalp (15 65 1 66 1 68 1 69 70))
    (with-buffer (make-buffer 9)
      (call-to-buffer 65 "B" :e "E" :d "D" :ff 70)))

  (nst:def-test unserializing-funcall-req
      (:equalp '(65 nil nil nil "e" 1 nil 66))
    (with-buffer (make-buffer 2)
      (call-to-buffer 65)
      (buffer-rewind)
      (nth-value 0 (call-from-buffer))))
  
  (nst:def-test unserializing-funcall-req+opt
      (:equalp '(65 "B" nil nil "e" 1 nil 66))
    (with-buffer (make-buffer 4)
      (call-to-buffer 65 "B")
      (buffer-rewind)
      (nth-value 0 (call-from-buffer))))
  
  (nst:def-test unserializing-funcall-req+opt+key
      (:equalp '(65 "B" (:e "E") nil "e" 1 nil 66))
    (with-buffer (make-buffer 6)
      (call-to-buffer 65 "B" :e "E")
      (buffer-rewind)
      (nth-value 0 (call-from-buffer))))
  
  (nst:def-test unserializing-funcall-all
      (:equalp '(65 "B" (:d "D" :e "E" :ff 70) "D" "E" 70 T 135))
    (with-buffer (make-buffer 9)
      (call-to-buffer 65 "B" :e "E" :d "D" :ff 70)
      (buffer-rewind)
      (nth-value 0 (call-from-buffer)))))
