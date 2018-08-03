;;;; package.lisp

(defpackage #:rtrace
  (:use #:cl #:stl)
  (:export #:create-scene
           #:add-sphere
           #:add-light
           #:make-rgb-color
           #:make-material
           #:make-sphere
           #:make-point
           #:make-light
           #:run-tests
           #:test-trace
           #:test-stl
           #:rtrace))
