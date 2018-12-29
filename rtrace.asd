;;;; rtrace.asd

(asdf:defsystem #:rtrace
  :description "A simple ray tracer written in Common Lisp."
  :author "Jeremiah LaRocco <jeremiah_larocco@fastmail.com>"
  :license "ISC (BSD like)"
  :depends-on (#:png #:vmath)
  :serial t
  :components ((:file "package")
               (:file "rtrace")))

