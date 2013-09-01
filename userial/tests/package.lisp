;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(defpackage :userial-tests
  (:use :cl :userial)
  (:export :run-tests))

(in-package :userial-tests)

(defun run-tests ()
  (let ((*print-pretty* t))
    (nst:nst-cmd :run-package #.*package*)))
