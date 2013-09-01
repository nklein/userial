;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(asdf:defsystem :userial
  :description
    "userial: a serialization library for binary message encoding."
  :version "0.8.2011.06.02"
  :author "Patrick Stein <pat@nklein.com>"
  :licence "MIT"
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

(defmethod asdf:perform ((op asdf:test-op)
                         (system (eql (asdf:find-system :userial))))
  (asdf:load-system :userial-tests)
  (funcall (find-symbol (symbol-name :run-tests) :userial-tests)))
