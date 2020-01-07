;;;; package.lisp

(defpackage #:vector
  (:use #:cl #:iterate)
  (:export #:make-vec
           #:v-add
           #:v-neg
           #:v-sub
           #:v-mul
           #:v-div
           #:v-dot
           #:v-cross
           #:v-squared-length
           #:v-length
           #:v-normalize
           #:x #:y #:z))

(defpackage #:raytracer
  (:use #:cl #:iterate #:vector)
  (:export #:render))
