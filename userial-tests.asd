;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(asdf:defsystem :userial-tests
  :description "tests for userial serialization library"
  :version "0.8.20180807"
  :author "Patrick Stein <pat@nklein.com>"
  :licence "MIT"
  :encoding :utf-8
  :depends-on ((:version "userial" "0.8.20180807")
               "nst")
  :components ((:module "userial"
                :components ((:module "tests"
                              :serial t
                              :components ((:file "package")
                                           (:file "buffer")
                                           (:file "serialize")
                                           (:file "func")
                                           (:file "run")))))))
