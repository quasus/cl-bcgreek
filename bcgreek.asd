;;;; bcgreek.asd

(asdf:defsystem #:bcgreek
  :description "Converting beta code to polytonic greek."
  :author "Stanislav Kondratyev <kondratjevsk@gmail.com>"
  :license "CC0"
  :serial t
  :components ((:file "package")
               (:file "ct-util")
               (:file "greekdef")
               (:file "bc-case")
               (:file "bcgreek")))

; vim: ft=lisp
