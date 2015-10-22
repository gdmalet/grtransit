(asdf:defsystem :grtransit
  :name "GRTransit"
  :author "Giles Malet <gdmalet+grtransit@gmail.com>"
  :version "1.0.0"
  :maintainer "Giles Malet <gdmalet+grtransit@gmail.com>"
  :licence "GPL 2"
  :description "Grok Grand River Transit bus data."
  :serial t
  :depends-on ("jsown")
  :components ((:file "globals")
			   (:file "grtransit")
			   (:file "utilities")))
