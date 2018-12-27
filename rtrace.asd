;;;; rtrace.asd

(asdf:defsystem #:rtrace
  :description "A simple ray tracer written in Common Lisp."
  :author "Jeremiah LaRocco <jeremiah.larocco@gmail.com>"
  :license "ISC"
  :depends-on (#:png #:3d-vectors #:stl)
  :serial t
  :components ((:file "package")
               (:file "rtrace")))

