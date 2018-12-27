;;;; package.lisp

(defpackage #:rtrace
  (:use #:cl #:stl #:3d-vectors)
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
