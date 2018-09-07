;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(asdf:defsystem :userial
  :description
    "userial: a serialization library for binary message encoding."
  :version "0.8.20180807"
  :author "Patrick Stein <pat@nklein.com>"
  :licence "MIT"
  :in-order-to ((asdf:test-op (asdf:load-op :userial-tests)))
  :perform (asdf:test-op (o c)
             (uiop:symbol-call :userial-tests :run-tests))
  :depends-on ("ieee-floats" "trivial-utf-8" "contextl")
  :components ((:module "userial"
                :serial t
                :components ((:file "package")
                             (:file "buffer")
                             (:file "util")
                             (:file "serialize")
                             (:file "func")
                             (:file "peek")))
               (:static-file "README.mkdn")
               (:static-file "LICENSE.txt")))
