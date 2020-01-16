(declaim (optimize (speed 0) (space 0) (debug 3) (safety 3)))

(push #p"./" asdf:*central-registry*)
(ql:quickload :raytracer)

(defparameter *scene*
  (list (raytracer:make-sphere :centre (vector:make-vec :x -0.7 :y -0.4 :z -9.0)
                               :radius 3.0
                               :colour (getf raytracer:*colours* :blue))
        
        (raytracer:make-sphere :centre (vector:make-vec :x 0.8 :y 1.1 :z -3.0)
                               :radius 1.0
                               :colour (getf raytracer:*colours* :green))
        
        (raytracer:make-sphere :centre (vector:make-vec :x 0.5 :y 0.5 :z -2.0)
                               :radius 0.2
                               :colour (getf raytracer:*colours* :yellow))
        
        (raytracer:make-sphere :centre (vector:make-vec :x 0.2 :y 0.2 :z -7.0)
                               :radius 1.4
                               :colour (getf raytracer:*colours* :red))))

(raytracer:render "scene-test.ppm" 100 100 *scene*)
