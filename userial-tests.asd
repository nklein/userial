;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(asdf:defsystem :userial-tests
  :description "tests for userial serialization library"
  :version "0.5.2011.04.21"
  :author "Patrick Stein <pat@nklein.com>"
  :licence "MIT"
  :depends-on ("userial" "nst")
  :components ((:module "userial"
		:components ((:module "tests"
			      :serial t
			      :components ((:file "package")
					   (:file "buffer")
					   (:file "serialize")))))))
