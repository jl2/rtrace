;;;; package.lisp

(defpackage #:rtrace
  (:use #:cl)
  (:export #:create-scene
           #:add-sphere
           #:add-light
           #:make-rgb-color
           #:make-material
           #:make-sphere
           #:make-point
           #:make-light
           #:run-tests
           #:rtrace))
