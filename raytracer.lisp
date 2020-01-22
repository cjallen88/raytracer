(in-package :raytracer)

(defparameter *colours* (list :red (make-vec :x 1.0 :y 0.0 :z 0.0)
                              :yellow (make-vec :x 1.0 :y 1.0 :z 0.0)
                              :blue (make-vec :x 0.0 :y 0.0 :z 1.0)
                              :green (make-vec :x 0.0 :y 1.0 :z 0.0)
                              :black (make-vec :x 0.0 :y 0.0 :z 0.0)
                              :white (make-vec :x 1.0 :y 1.0 :z 1.0)
                              :orange (make-vec :x 1.0 :y 0.5 :z 0.0)))

(defparameter *background-colour* (getf *colours* :black))

(defparameter *ambient-light-intensity* 0.1)

(defparameter *samples* 50)

;;; SPHERE

(defclass sphere ()
  ((centre :initarg :centre :accessor centre)
   (radius :initarg :radius :accessor radius :type float)
   (colour :initarg :colour :accessor colour)
   (specular-exponent :initarg :specular-exponent :accessor specular-exponent :type float)))

(defun make-sphere (&key centre radius (colour (getf *colours* :yellow)) (specular-exp 0.4))
  (make-instance 'sphere :centre centre :radius radius :colour colour :specular-exponent specular-exp))

(defmethod print-object ((obj sphere) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (centre radius colour specular-exponent) obj
      (format stream "centre: ~a, radius: ~a, colour: ~a, specular-exp: ~a"
              centre radius colour specular-exponent))))

;;; POINT LIGHT

(defclass point-light ()
  ((pos :initarg :pos :accessor pos)
   (intensity :initarg :intensity :accessor intensity :type float)))

(defun make-point-light (&key pos (intensity 1.0))
  (make-instance 'point-light :pos pos :intensity intensity))

(defmethod print-object ((obj point-light) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (pos intensity) obj
      (format stream "position: ~a, intensity: ~a" pos intensity))))

;;; RAY

(defclass ray ()
  ((origin :initarg :origin :accessor origin)
   (direction :initarg :direction :accessor direction)))

(defun make-ray (&key origin direction)
  (declare (vec origin direction))
  (make-instance 'ray :origin origin :direction direction))

(defmethod print-object ((obj ray) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (origin direction) obj
      (format stream "origin: ~a direction: ~a" origin direction))))

(defmethod intersect ((ray ray) (sphere sphere))
  "Returns the intersect entrance if RAY intersects with SPHERE, else NIL
https://www.scratchapixel.com/lessons/3d-basic-rendering/minimal-ray-tracer-rendering-simple-shapes/ray-sphere-intersection"
  (let* ((v-from-o-to-c (v-sub (centre sphere) (origin ray)))
         (tca (v-dot v-from-o-to-c (direction ray)))
         (d-sqrd (- (v-dot v-from-o-to-c v-from-o-to-c)
                    (* tca tca)))
         (sphere-behind-ray (< tca 0)))
    (if sphere-behind-ray
        (let ((l (v-length v-from-o-to-c)))
          (when (<= l (radius sphere))
            l)) ; is origin inside sphere
        ;; else centre of sphere projects on the array
        (let ((radius-sqrd (expt (radius sphere) 2)))
          (unless (> d-sqrd radius-sqrd)
            (let* ((thc (sqrt (- radius-sqrd d-sqrd)))
                   (t0 (- tca thc))  ; intersect front
                   (t1 (+ tca thc))) ; intersect back
              (when (> t0 t1) (rotatef t0 t1))
              (when (> t0 0)
                t0)))))))

(defun reflect (light-dir hit-normal)
  (v-sub light-dir (v-mul (v-mul hit-normal (v-dot light-dir hit-normal))
                          2.0)))

(defun hit-colour (ray hit-obj hit-point hit-normal scene lights)
  (flet (;; https://github.com/ssloy/tinyraytracer/commit/9a728fff2bbebb1eedd86e1ac89f657d43191609
         ;; https://www.youtube.com/watch?v=5apJJKd4z-
         (diffuse-intensity (light light-dir)
           (* (intensity light)
              (max 0.0 (min 1.0 (v-dot hit-normal light-dir)))))
         ;; http://learnwebgl.brown37.net/09_lights/lights_specular.htm
         (specular-intensity (light-dir)
           (* (expt (max 0.0 ;; sets the reflect on the back half to 0
                         (v-dot (reflect light-dir hit-normal)
                                (direction ray)))
                    50)))
         (shadowed (light-dir light-distance)
           (let* ((shadow-origin (if (< (v-length (v-mul light-dir hit-normal)) 0)
                                     (v-sub hit-point (v-mul hit-normal 1e-3))
                                     (v-add hit-point (v-mul hit-normal 1e-3))))
                  (shadow-ray (make-ray :origin shadow-origin :direction light-dir)))
             (iter (for obj in scene)
               (let ((shadow-hit-dist (intersect shadow-ray obj)))
                 (thereis (and shadow-hit-dist
                               (let* ((shadow-hit-point (v-add shadow-origin (v-mul light-dir shadow-hit-dist)))
                                      (shadow-distance (v-length (v-sub shadow-hit-point shadow-origin))))
                                 (< shadow-distance light-distance)))))))))
    (iter (for l in lights)
      (let ((light-dir (v-normalize (v-sub (pos l) hit-point)))
            (light-distance (v-length (v-sub (pos l) hit-point))))
        (unless (shadowed light-dir light-distance)
          (sum (diffuse-intensity l light-dir) into di)
          (sum (specular-intensity light-dir) into si))
        (finally (return (v-add (v-add (v-mul (colour hit-obj) (float di))
                                       (v-mul (colour hit-obj) *ambient-light-intensity*))
                                (* (float si) (specular-exponent hit-obj)))))))))

(defun cast-ray (ray scene lights)
  "Returns the colour of the ray depending on whether it hits anything"
  (let* ((hits (mapcar (lambda (obj)
                         (list :hit-obj obj :hit-distance (intersect ray obj)))
                       scene))
         (closest
           (iter (for h in hits)
             (when (getf h :hit-distance)
               (finding h minimizing (getf h :hit-distance))))))
    (if closest
        (let* ((hit-point (v-add (origin ray)
                                 (v-mul (direction ray) (getf closest :hit-distance))))
               (hit-normal (v-normalize (v-sub hit-point
                                               (centre (getf closest :hit-obj))))))
          (hit-colour ray (getf closest :hit-obj) hit-point hit-normal scene lights))
        *background-colour*)))

(defun raster-to-camera-coord (axis-pos axis-scale x-or-y)
  "https://www.scratchapixel.com/lessons/3d-basic-rendering/ray-tracing-generating-camera-rays/generating-camera-rays
antialiasing: https://raytracing.github.io/books/RayTracingInOneWeekend.html#antialiasing"
  (flet ((deg-to-rad (degrees) (* pi (/ degrees 180.0)))
         (rad-to-deg (radians) (/ (* radians 180.0) pi)))
    (let* ((ndc ; Normalized Device Coordinates, mapping raster space to world space [0,1]
             (/ (+ axis-pos (random 1.0)) ; random for supersampling
                (float axis-scale)))
           (screenspace-coord ; convert x ndc to screenspace [-1,1]
             (ecase x-or-y
               (:x (- (* 2 ndc) 1))
               (:y (- 1 (* 2 ndc)))))
           (fov-angle 90)
           (scale (tan (deg-to-rad (* fov-angle 0.5))))
           (coord (* screenspace-coord scale)))
      coord)))

(defun print-colour (s vec)
  "Calculate colour from given vector, clamps max values to 255
to prevent issues when diffuse light intensity is too high"
  (format s "~a ~a ~a~&"
          (min 255 (floor (* (x vec) 255)))
          (min 255 (floor (* (y vec) 255)))
          (min 255 (floor (* (z vec) 255)))))

(defun print-fb (filename w h fb)
  (with-open-file (s filename :direction :output
                              :if-exists :supersede)
    (format s "P3~&~a ~a~&255~&" w h)
    (iter (for i from 0 to (1- (* w h)))  
      (print-colour s (aref fb i)))))

(defun render (filename width height scene lights)
  (let ((camera (make-vec :x 0.0 :y 0.0 :z 0.0)))
    (let ((fb (make-array (* width height))))
      (iter (for j from 0 to (1- height))
        (iter (for i from 0 to (1- width))
          (let ((total-colour (make-vec :x 0.0 :y 0.0 :z 0.0)))
            (iter (for s from 0 to *samples*)
              (let* (;; map each x/y coord to ndc (map to real world) to calculate direction of ray
                     (x (* (raster-to-camera-coord i width :x)
                           (/ width height))) ; correct for aspect ratio, assuming width > height
                     (y (raster-to-camera-coord j height :y))
                     (dir (v-normalize (make-vec :x x :y y :z -1.0)))
                     (ray (make-ray :origin camera :direction dir)))
                (setf total-colour
                      (v-add total-colour (cast-ray ray scene lights)))
                (setf (aref fb (+ i (* j width)))
                      (v-div total-colour (float *samples*))))))))
      (print-fb filename width height fb))))
