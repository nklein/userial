
(asdf:defsystem :userial-tests
  :description "tests for userial serialization library"
  :version "0.1.2010.12.26"
  :author "Patrick Stein <pat@nklein.com>"
  :license "Public Domain"
  :depends-on ("userial" "nst")
  :components ((:module "userial"
		:components ((:module "tests"
			      :serial t
			      :components ((:file "package")
					   (:file "buffer")
					   (:file "serialize")))))))
