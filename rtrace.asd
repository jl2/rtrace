;;;; rtrace.asd

(asdf:defsystem #:rtrace
  :description "A simple ray tracer written in Common Lisp."
  :author "Jeremiah LaRocco <jeremiah.larocco@gmail.com>"
  :license "ISC (BSD like)"
  :depends-on (#:png)
  :serial t
  :components ((:file "package")
               (:file "rtrace")))

