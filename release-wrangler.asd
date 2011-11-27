;;;; release-wrangler.asd

(asdf:defsystem #:release-wrangler
  :serial t
  :author "Zach Beane <xach@xach.com>"
  :description "Prepare & release one of my CL projects."
  :depends-on (#:commando
               #:split-sequence)
  :components ((:file "package")
               (:file "commands")
               (:file "date")
               (:file "versions")
               (:file "project")
               (:file "release-wrangler")))

