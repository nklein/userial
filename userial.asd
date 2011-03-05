;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(asdf:defsystem :userial
  :description
    "userial: a serialization library for binary message encoding."
  :version "0.3.2011.03.05"
  :author "Patrick Stein <pat@nklein.com>"
  :license "Public Domain"
  :depends-on ("ieee-floats" "trivial-utf-8")
  :components ((:module "userial"
                :serial t
		:components ((:file "package")
			     (:file "buffer")
			     (:file "serialize")))))
