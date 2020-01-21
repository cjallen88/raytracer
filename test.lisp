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
                               :colour (getf raytracer:*colours* :red))
        
        (raytracer:make-sphere :centre (vector:make-vec :x -0.0 :y -14.0 :z -9.0)
                               :radius 12.0
                               :colour (getf raytracer:*colours* :orange))))

(defparameter *lights*
  (list (raytracer:make-point-light :pos (vector:make-vec :x -20.0 :y 20.0 :z 20.0)
                                    :intensity 0.5)
        
        (raytracer:make-point-light :pos (vector:make-vec :x 30.0 :y 50.0 :z -25.0)
                                    :intensity 0.5)

        (raytracer:make-point-light :pos (vector:make-vec :x 30.0 :y 20.0 :z 30.0)
                                    :intensity 0.5)))

(raytracer:render "scene-test.ppm" 500 500 *scene* *lights*)
