(declaim (optimize (speed 0) (space 0) (debug 3)))

(ql:quickload 'iterate)
(use-package 'iterate)

(defclass vec ()
  ((x :initarg :x :accessor x)
   (y :initarg :y :accessor y)
   (z :initarg :z :accessor z)))

(defun make-vec (&key x y z)
  (make-instance 'vec :x x :y y :z z))

(defmethod print-object ((obj vec) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (x y z) obj
      (format stream "x: ~a, y: ~a, z: ~a" x y z))))

(defun v-add (v1 v2)
  "Compute addition of two vectors"
  (make-vec :x (+ (x v1) (x v2))
            :y (+ (y v1) (y v2))
            :z (+ (z v1) (z v2))))

(defun v-neg (v)
  "Invert a vector"
  (with-slots (x y z) v
    (make-vec :x (- x) :y (- y) :z (- z))))

(defun v-sub (v1 v2)
  "Compute difference of two vectors"
  (make-vec :x (- (x v1) (x v2))
            :y (- (y v1) (y v2))
            :z (- (z v1) (z v2))))

(defmethod  v-mul ((v1 vec) (v2 vec))
  "Compute product of two vectors"
  (make-vec :x (* (x v1) (x v2))
            :y (* (y v1) (y v2))
            :z (* (z v1) (z v2))))

(defmethod v-mul ((v vec) number)
  "Compute product of a vector and a float"
  (make-vec :x (* (x v) number)
            :y (* (y v) number)
            :z (* (z v) number)))

(defmethod v-mul (number (v vec))
  (v-mul v number))

(defmethod  v-div ((v1 vec) (v2 vec))
  "Compute division of two vectors"
  (make-vec :x (/ (x v1) (x v2))
            :y (/ (y v1) (y v2))
            :z (/ (z v1) (z v2))))

(defmethod v-div ((v vec) number)
  "Compute division of a vector and a float"
  (make-vec :x (/ (x v) number)
            :y (/ (y v) number)
            :z (/ (z v) number)))

(defmethod v-div (number (v vec))
  (make-vec :x (/ number (x v))
            :y (/ number (y v))
            :z (/ number (z v))))

(defun v-dot (v1 v2)
  "Computes dot product (or inner product) of two vectors.
Produces a scalar which tells you how close to being orthoganal the vecs are,
where 0 is orthoganal, positive indicates closer than orthoganal (i.e. that they project on one another) and negative the opposite"
  (+ (* (x v1)
        (x v2))
     (* (y v1)
        (y v2))
     (* (z v1)
        (z v2))))

(defun v-cross (v1 v2)
  "Compute cross product of two vectors.
Produces a vector perpendicular to the plane defined by v1 and v2.

Used in lighting and finding angles between two vectors.

Assuming that v1 and v2 are in the XZ plane, then the direction of the cross product vector will point upwards if the rotation from v1 to v2 is counter clockwise, and downwards if the rotation is clockwise."
  (make-vec :x (- (* (y v1) (z v2))
                  (* (z v1) (y v2)))
            :y (- (* (z v1) (x v2))
                  (* (x v1) (z v2)))
            :z (- (* (x v1) (y v2))
                  (* (y v1) (x v2)))))

(defun v-squared-length (v)
  "Compute the squared length of a vector"
  (with-slots (x y z) v
    (+ (* x x)
       (* y y)
       (* z z))))

(defun v-length (v)
  "Compute the length of a vector"
  (sqrt (v-squared-length v)))

(defun v-normalize (v)
  "Normalize a vector"
  (v-div v (v-length v)))

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
  (make-instance 'ray :origin origin :direction direction))

(defmethod print-object ((obj ray) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (origin direction) obj
      (format stream "origin: ~a direction: ~a" origin direction))))

(defmethod intersect ((ray ray) (sphere sphere))
  (with-slots (origin direction) ray
    (with-slots (centre radius) sphere
      (let* ((v-from-o-to-c (v-sub centre origin))
             (sphere-behind-ray (< (v-dot v-from-o-to-c direction) 0)))
        (if sphere-behind-ray
            (let ((l (v-length v-from-o-to-c)))
              (cond ((> l radius) nil) ; origin outside sphere
                    ((= l radius) origin) ; origin on circumference
                    ;; origin inside sphere
                    (t (let* ((projection (v-add centre direction))
                              (v-of-proj-to-centre (v-sub centre projection))
                              (v-of-proj-to-origin (v-sub origin projection))
                              (distance-to-centre (sqrt (- (expt radius 2)
                                                           (expt (v-length v-of-proj-to-centre) 2))))
                              (distance-to-intersection (- distance-to-centre
                                                           (v-length v-of-proj-to-origin))))
                         (v-add origin (v-mul direction distance-to-intersection)))))) 
            ;; else centre of sphere projects on the array
            (let* ((projection (v-add centre direction))
                   (v-of-centre-to-projection (v-sub projection centre))
                   (v-of-proj-to-centre (v-sub centre projection))
                   (v-of-proj-to-origin (v-sub origin projection))
                   (origin-outside-sphere (> (v-length v-of-centre-to-projection) radius)))
              (if origin-outside-sphere
                  nil
                  (let* ((distance-to-centre (sqrt (- (expt radius 2)
                                                      (expt (v-length v-of-proj-to-centre) 2))))
                         (distance-to-intersection
                           (if (> (v-length v-from-o-to-c) radius)
                               (- (v-length v-of-proj-to-origin)
                                  distance-to-centre)
                               (+ (v-length v-of-proj-to-origin)
                                  distance-to-centre))))
                    (v-add origin (v-mul direction distance-to-intersection))))))))))

(defun cast-ray (ray sphere)
  (if (intersect ray sphere)
      (make-vec :x 0.4 :y 0.4 :z 0.3)
      (make-vec :x 0.2 :y 0.7 :z 0.8)))

;; add framebuffer code

(defun print-ppm (filename width height)
  (with-open-file (s filename :direction :output
                              :if-exists :supersede)
    (format s "P3~&~a ~a~&255~&" width height)
    (iter (for j from (1- height) downto 0)
      (iter (for i from 0 to (1- width))
        (let* ((camera (make-vec :x 0 :y 0 :z 0))
               (fov-angle (/ 2 pi))
               (sphere (make-sphere :centre (make-vec :x -3 :y 0 :z -16)
                                    :radius 2.0))
               (x ; (2*(i + 0.5)/(float)width  - 1)*tan(fov/2.)*width/(float)height;
                 (* (* 2 (/ (+ i 0.5) (1- width)))
                    (tan (/ fov-angle 2))
                    (/ width height)))
               (y ; -(2*(j + 0.5)/(float)height - 1)*tan(fov/2.)
                 (- (* 2 (/ (+ j 0.5) (1- height))
                       (tan (/ fov-angle 2)))))
               (dir (v-normalize (make-vec :x x :y y :z -1)))
               (r (make-ray :origin camera :direction dir))
               (c (cast-ray r sphere)))
          (format s "~a ~a ~a~&"
                  (floor (* (x c) 255))
                  (floor (* (y c) 255))
                  (floor (* (z c) 255))))))))
