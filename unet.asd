
(asdf:defsystem :unet
  :description
    "unet: a networking library with per-message control over reliability."
  :version "0.1.2010.12.21"
  :author "Patrick Stein <pat@nklein.com>"
  :license "Public Domain"
  :depends-on ("usocket" "ieee-floats" "trivial-utf-8")
  :components ((:module "unet"
                :serial t
		:components ((:file "package")
			     (:file "packet")
			     (:file "serialize")
			     (:file "class")))))
