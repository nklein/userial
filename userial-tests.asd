;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(asdf:defsystem :userial-tests
  :description "tests for userial serialization library"
  :version "0.8.2011.06.02"
  :author "Patrick Stein <pat@nklein.com>"
  :licence "MIT"
  :encoding :utf-8
  :depends-on ("userial" "nst")
  :components ((:module "userial"
                :components ((:module "tests"
                              :serial t
                              :components ((:file "package")
                                           (:file "buffer")
                                           (:file "serialize")
                                           (:file "func")))))))
