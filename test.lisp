(push #p"./" asdf:*central-registry*)
(ql:quickload :raytracer)
(raytracer:render "first.ppm" 100 100)
