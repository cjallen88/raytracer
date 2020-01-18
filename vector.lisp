(in-package :vector)

(defclass vec ()
  ((x :initarg :x :accessor x :type float)
   (y :initarg :y :accessor y :type float)
   (z :initarg :z :accessor z :type float)))

(defun make-vec (&key x y z)
  (declare (float x y z))
  (make-instance 'vec :x x :y y :z z))

(defmethod print-object ((obj vec) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (x y z) obj
      (format stream "x: ~a, y: ~a, z: ~a" x y z))))

(defun v-add (v1 v2)
  "Compute addition of two vectors"
  (declare (vec v1 v2))
  (make-vec :x (+ (x v1) (x v2))
            :y (+ (y v1) (y v2))
            :z (+ (z v1) (z v2))))

(defun v-neg (v)
  "Invert a vector"
  (declare (vec v))
  (with-slots (x y z) v
    (make-vec :x (- x) :y (- y) :z (- z))))

(defun v-sub (v1 v2)
  "Compute difference of two vectors"
  (declare (vec v1 v2))
  (make-vec :x (- (x v1) (x v2))
            :y (- (y v1) (y v2))
            :z (- (z v1) (z v2))))

(defmethod  v-mul ((v1 vec) (v2 vec))
  "Compute product of two vectors"
  (make-vec :x (* (x v1) (x v2))
            :y (* (y v1) (y v2))
            :z (* (z v1) (z v2))))

(defmethod v-mul ((v vec) (f float))
  "Compute product of a vector and a float"
  (make-vec :x (* (x v) f)
            :y (* (y v) f)
            :z (* (z v) f)))

(defmethod v-mul ((f float) (v vec))
  (v-mul v f))

(defmethod  v-div ((v1 vec) (v2 vec))
  "Compute division of two vectors"
  (make-vec :x (/ (x v1) (x v2))
            :y (/ (y v1) (y v2))
            :z (/ (z v1) (z v2))))

(defmethod v-div ((v vec) (f float))
  "Compute division of a vector and a float"
  (make-vec :x (/ (x v) f)
            :y (/ (y v) f)
            :z (/ (z v) f)))

(defmethod v-div ((f float) (v vec))
  (make-vec :x (/ f (x v))
            :y (/ f (y v))
            :z (/ f (z v))))

(defun v-dot (v1 v2)
  "Computes dot product (or inner product) of two vectors.
Produces a scalar which tells you how close to being orthoganal the vecs are,
where 0 is orthoganal, positive indicates closer than orthoganal (i.e. that they project on one another) and negative the opposite"
  (declare (vec v1 v2))
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
  (declare (vec v1 v2))
  (make-vec :x (- (* (y v1) (z v2))
                  (* (z v1) (y v2)))
            :y (- (* (z v1) (x v2))
                  (* (x v1) (z v2)))
            :z (- (* (x v1) (y v2))
                  (* (y v1) (x v2)))))

(defun v-squared-length (v)
  "Compute the squared length of a vector"
  (declare (vec v))
  (with-slots (x y z) v
    (+ (* x x)
       (* y y)
       (* z z))))

(defun v-length (v)
  "Compute the length of a vector"
  (declare (vec v))
  (sqrt (v-squared-length v)))

(defun v-normalize (v)
  "Normalize a vector"
  (declare (vec v))
  (let ((l (v-length v)))
    (if (zerop l)
        v
        (v-div v l))))
