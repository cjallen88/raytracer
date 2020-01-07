(in-package :raytracer)

(declaim (optimize (speed 0) (space 0) (debug 3) (safety 3)))

(ql:quickload 'iterate)
(use-package 'iterate)

(defclass sphere ()
  ((centre :initarg :centre :accessor centre)
   (radius :initarg :radius :accessor radius)))

(defun make-sphere (&key centre radius)
  (make-instance 'sphere :centre centre :radius radius))

(defmethod print-object ((obj sphere) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (centre radius) obj
      (format stream "centre: ~a, radius: ~a" centre radius))))

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
  "Returns T if RAY intersects with SPHERE
https://www.scratchapixel.com/lessons/3d-basic-rendering/minimal-ray-tracer-rendering-simple-shapes/ray-sphere-intersection"
  (with-slots (origin direction) ray
    (with-slots (centre radius) sphere
      (let* ((v-from-o-to-c (v-sub centre origin))
             (tca (v-dot v-from-o-to-c direction)) ; distance to projection of centre on ray
             (d-sqrd (- (v-dot v-from-o-to-c v-from-o-to-c)
                        (* tca tca)))
             (sphere-behind-ray (< tca 0)))
        (if sphere-behind-ray
            (let ((l (v-length v-from-o-to-c)))
              (<= l radius)) ; is origin inside sphere
            ;; else centre of sphere projects on the array
            (let ((radius-sqrd (expt radius 2)))
              (if (> d-sqrd radius-sqrd)
                  nil
                  (let* ((thc (sqrt (- radius-sqrd d-sqrd)))
                         (t0 (- tca thc))  ; intersect front
                         (t1 (+ tca thc))) ; intersect back
                    (when (> t0 t1) (rotatef t0 t1))
                    (> t0 0)))))))))

(defun cast-ray (ray sphere)
  (if (intersect ray sphere)
      (make-vec :x 1.0 :y 1.0 :z 0.0)   ; yellow
      (make-vec :x 0.0 :y 0.5 :z 1.0))) ; blue

(defun deg-to-rad (degrees) (* pi (/ degrees 180.0)))
(defun rad-to-deg (radians) (/ (* radians 180.0) pi)) 

(defun raster-to-camera-coord (axis-pos axis-scale x-or-y)
  "https://www.scratchapixel.com/lessons/3d-basic-rendering/ray-tracing-generating-camera-rays/generating-camera-rays"
  (let* ((ndc ; Normalized Device Coordinates, mapping raster space to world space [0,1]
           (/ (+ axis-pos 0.5) ; 0.5 to push position to centre of pixel
              (float axis-scale)))
         (screenspace-coord ; convert x ndc to screenspace [-1,1]
           (ecase x-or-y
             (:x (- (* 2 ndc) 1))
             (:y (- 1 (* 2 ndc)))))
         (fov-angle 90)
         (scale (tan (deg-to-rad (* fov-angle 0.5))))
         (coord (* screenspace-coord scale)))
    coord))

(defun print-colour (s vec)
  (format s "~a ~a ~a~&"
          (floor (* (x vec) 255))
          (floor (* (y vec) 255))
          (floor (* (z vec) 255))))

(defun print-fb (filename w h fb)
  (with-open-file (s filename :direction :output
                              :if-exists :supersede)
    (format s "P3~&~a ~a~&255~&" w h)
    (iter (for i from 0 to (1- (* w h)))  
      (print-colour s (aref fb i)))))

(defun render (filename width height)
  (let ((fb (make-array (* width height))))
    (iter (for j from 0 to (1- height))
      (iter (for i from 0 to (1- width))
        (let* ((camera (make-vec :x 0.0 :y 0.0 :z 0.0))
               (sphere (make-sphere :centre (make-vec :x 0.0 :y 0.0 :z -8.0)
                                    :radius 2.0))
               ;; map each x/y coord to ndc (map to real world) to calculate direction of ray
               (x (* (raster-to-camera-coord i width :x)
                     (/ width height))) ; correct for aspect ratio, assuming width > height
               (y (raster-to-camera-coord j height :y))
               (dir (v-normalize (make-vec :x x :y y :z -1.0)))
               (ray (make-ray :origin camera :direction dir)))
          (setf (aref fb (+ i (* j width)))
                (cast-ray ray sphere)))))
    (print-fb filename width height fb)))
