;;;; rtrace.asd

(asdf:defsystem #:rtrace
  :description "A simple ray tracer written in Common Lisp."
  :author "Jeremiah LaRocco <jeremiah.larocco@gmail.com>"
  :license "ISC"
  :depends-on (#:png)
  :serial t
  :components ((:file "package")
               (:file "rtrace")))

