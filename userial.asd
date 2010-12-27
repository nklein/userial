
(asdf:defsystem :userial
  :description
    "userial: a serialization library for binary message encoding."
  :version "0.1.2010.12.26"
  :author "Patrick Stein <pat@nklein.com>"
  :license "Public Domain"
  :depends-on ("ieee-floats" "trivial-utf-8")
  :components ((:module "userial"
                :serial t
		:components ((:file "package")
			     (:file "buffer")
			     (:file "serialize")))))
