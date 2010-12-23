
(asdf:defsystem :unet-tests
  :description "tests for unet networking library"
  :version "0.1.2010.12.21"
  :author "Patrick Stein <pat@nklein.com>"
  :license "Public Domain"
  :depends-on ("unet" "nst")
  :components ((:module "unet"
		:components ((:module "tests"
			      :serial t
			      :components ((:file "package")
					   (:file "packet")
					   (:file "serialize")))))))
