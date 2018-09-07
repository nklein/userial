;; Copyright (c) 2011-2018 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :userial-tests)

(defun run-tests ()
  (let ((*print-pretty* t))
    (nst:nst-cmd :run-package #.*package*)))
