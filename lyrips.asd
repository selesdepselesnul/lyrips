;;;; lyrips.asd

(asdf:defsystem #:lyrips
  :description "lyrips"
  :author "Moch Deden <moch.deden.r@gmail.com>"
  :license ""
  :depends-on (#dexador
               #plump
               #lquery
               #lparallel
               #cl-ppcre
               #cl-strings)
  :serial t
  :components ((:file "package")
               (:file "lyrips")))

