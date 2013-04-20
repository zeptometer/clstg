(in-package :asdf)

(defsystem "shooting"
  :version "0.0.1"
  :author  "zeptometer"
  :depends-on (:iterate
	       :lispbuilder-sdl)
  :components ((:file "utils")
	       (:file "core" :depends-on ("utils"))))
