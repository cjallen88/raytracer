;;;; raytracer.asd

(asdf:defsystem #:raytracer
  :description "A small raytracer"
  :author "Craig Allen <cjallen88@gmail.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:iterate)
  :components ((:file "package")
               (:file "vector")
               (:file "raytracer")))
