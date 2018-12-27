;;;; rtrace.lisp

(in-package #:rtrace)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defstruct ray
  (origin (vec3 0.0 0.0 0.0) :type vec3)
  (direction (vec3 0.0 0.0 0.0) :type vec3))

(defstruct material
  (kd (vec3 0.0 0.0 0.0) :type vec3)
  (ks (vec3 0.0 0.0 0.0) :type vec3)
  (ka (vec3 0.1 0.1 0.1) :type vec3)
  (kr (vec3 0.0 0.0 0.0) :type vec3)
  (kt (vec3 0.0 0.0 0.0) :type vec3)
  (ior 1.0 :type double-float))

(defstruct sphere
  (radius 1.0 :type double-float)
  (location (vec3 0.0 0.0 0.0) :type vec3)
  (material (make-material) :type material))

(defstruct rstl
  (tris (make-array 0) :type simple-vector)
  (material (make-material) :type material))

(defgeneric material (obj)
  (:documentation "Return an object's material."))

(defmethod material ((obj sphere))
  (sphere-material obj))

(defmethod material ((obj rstl))
  (rstl-material obj))

(defstruct rintersection
  (tval 999999999.0 :type double-float)
  (intersects nil :type boolean)
  (object nil)
  (ray nil)
  (point nil)
  (normal nil))

(defun point-on-ray (r tv)
  (v+ (ray-origin r) (v* tv (ray-direction r))))

(defgeneric intersects (r obj)
  (:documentation "Compute an intersection for a ray and object."))

(defmethod intersects (r (s sphere))
  (let* ((oc (v- (ray-origin r) (sphere-location s)))
         (rad (sphere-radius s))
         (lenoc (vlength oc))
         (ldoc (v. (ray-direction r) oc))
         (lenl (vlength (ray-direction r)))
         (under  (- (* ldoc ldoc) (* (* lenl lenl) (- (* lenoc lenoc) (* rad rad))))))
    (declare (type vec3 oc)
             (type double-float rad lenoc ldoc lenl under))
    (cond 
          ((= under 0.0)
           (let* ((tv (/ 
                       (- (sqrt under) ldoc)
                       (* lenl lenl)))
                 (pt (point-on-ray r tv)))
             (declare (type vec3 pt)
                      (type double-float tv))
             (make-rintersection
              :tval tv
              :intersects t
              :object s :ray r
              :normal (vunit (v- (sphere-location s) pt))
              :point pt)))

          ((> under 0.0)
           (let* ((tv (/ (min (+ (- ldoc) (sqrt under))
                              (- (- ldoc) (sqrt under))) (* lenl lenl)))
                  (pt (point-on-ray r tv)))
             (declare (type vec3 pt)
                      (type double-float tv))
             (make-rintersection
              :tval tv
              :intersects t
              :object s :ray r
              :point pt
              :normal (vunit (v- (sphere-location s) pt)))))
          (t
           nil))))

(defmethod equal-tol (a b &optional (tol 0.0001))
  (< (abs (- a b)) tol))

(defmethod intersects (r (tri stl:triangle))
  (with-slots (normal pt1 pt2 pt3) tri
  (let* (
         (e1 (v- pt2 pt1))
           (e2 (v- pt3 pt1))
           (p (vc (ray-direction r) e2))
           (a (v. e1 p)))
      (if (equal-tol a 0.0)
          (return-from intersects nil))
      (let* ((f (/ 1.0 a))
             (s (v- (ray-origin r) pt1))
             (u (* f (v. s p))))
        (if (or (< u 0.0) (> u 1.0))
            (return-from intersects nil))
        (let* ((q (vc s e1))
               (v (* f (v. (ray-direction r) q))))
          (when (or (< v 0.0) (> (+ u v) 1.0))
            (return-from intersects nil))
          (make-rintersection :tval (* f (v. e2 q))
                              :intersects t
                              :object tri
                              :ray r
                              :point (point-on-ray r (* f (v. e2 q)))
                              :normal (vc e2 e1);; (triangle-normal tri)
                              ))))))


(defmethod intersects (r (tris rstl))
  (let ((closest nil))
    (loop for tri across (rstl-tris tris)
         for next = (intersects r tri) then (intersects r tri)
         do
         (when next
           (if (null closest)
               (setf closest next)
               (when (< (rintersection-tval next) (rintersection-tval closest))
                 (setf closest next)))
           (setf (rintersection-object closest) tris)))
    closest))


(defstruct light
  (location (vec3 0.0 0.0 0.0) :type vec3)
  (color (vec3 0.0 0.0 0.0) :type vec3))

(defstruct scene
  (eye-point (vec3 10.0 10.0 10.0) :type vec3)
  (look-at (vec3 0.0 0.0 0.0) :type vec3)
  (up (vec3 0.0 1.0 0.0))
  (aspect (/ 4.0 3.0) :type double-float)
  (objects nil :type list)
  (lights nil :type list))

(defun create-scene ()
  (make-scene))

(defun add-object (scn sph)
  (push sph (scene-objects scn)))

(defun add-light (scn lght)
  (push lght (scene-lights scn) ))

(defun compute-eye-ray (origin look-at up aspect x y width height)
  (declare (type vec3 origin look-at)
           (type vec3 up)
           (type (signed-byte 64) x y width height)
           (type double-float aspect))
  (let* ((pdiff (vunit (v- origin look-at)))
         (right (vc up pdiff))
         (rc (v* (* aspect (+ -0.5 (/ x 1.0 width))) right))
         (uc (v* (+ -0.5 (/ y 1.0 height)) up))
         (vdir (v+ pdiff rc uc)))
    (declare (type vec3 pdiff right rc uc vdir))
    (make-ray :origin origin
              :direction (if (= 0.0 (vlength vdir))
                             vdir
                             (vunit vdir)))))


(defun clamp (val &key (max 1.0) (min 0.0))
  (declare (type double-float val max min))
  (cond ((< val min) min)
        ((> val max) max)
        (t val)))


(defun set-pixel (img x y rgb)
  (let ((r (truncate (* 255 (vx rgb))))
        (g (truncate (* 255 (vy rgb))))
        (b (truncate (* 255 (vz rgb)))))
    (declare (type fixnum r g b)
             (type (simple-array  (unsigned-byte 8) *) img)
             (type fixnum x y))
    (setf (aref img y x 0) r)
    (setf (aref img y x 1) g)
    (setf (aref img y x 2) b)))

(defun shade (scn isect)
  (declare (type scene scn)
           (type (or null rintersection) isect))
    (when (null isect)
      (return-from shade (vec3 0.0
                               0.0
                               0.0)))
    (let ((ry (rintersection-ray isect)))
      (declare (type ray ry)
               (ignorable ry))
      (let ((rval (vec3 0.0 0.0 0.0)))
        (declare (type vec3 rval))
        (dolist (lght (scene-lights scn))
          (declare (type light lght))
          (let*  ((ldir (v- (light-location lght) (rintersection-point isect)))
                  ;; (ldist (vlength ldir))
                  (nldir (vunit ldir))
                  (ndl (v. nldir (rintersection-normal isect)))
                  (intensity (clamp ndl)))
            (declare (type vec3 ldir nldir)
                     (type double-float ndl intensity))
            (setf rval (v+ rval (v* intensity (v* (light-color lght) (material-kd (material (rintersection-object isect)))))))))
        rval)))

(defun itrace (scn ry)
  (declare (type scene scn)
           (type ray ry))
  (let ((nearest nil))
    (declare (type (or null rintersection) nearest))
    (dolist (obj (scene-objects scn))
      (let ((isect (intersects ry obj)))
        (declare (type (or null rintersection) isect))
        (if (and (null nearest) isect)
            (setf nearest isect)
            (if (and isect (< (rintersection-tval isect) (rintersection-tval nearest)))
                (setf nearest isect)))))
    (shade scn nearest) ))

(defun rtrace (scn file-name &key (width 800) (height 600))
  (declare (type scene scn)
           (type string file-name)
           (type fixnum width height))
  (let ((img (png:make-image height width 3 8)))
    (dotimes (j (- height 1))
      (declare (type fixnum j))
      (dotimes (i (- width 1))
        (declare (type fixnum i))
        (let ((r (compute-eye-ray (scene-eye-point scn)
                                  (scene-look-at scn)
                                  (scene-up scn)
                                  (scene-aspect scn)
                                  i j
                                  width height)))
          (declare (type ray r))
          (set-pixel img i j (itrace scn r)))))
    (with-open-file (output file-name :element-type '(unsigned-byte 8) :direction :output :if-exists :supersede)
                    (png:encode img output))))

(defun run-tests ()
  (let ((v1 (vec3 1.0 0.0 0.0))
        (v2 (vec3 0.0 1.0 0.0))
        (v3 (vec3 10.0 10.0 10.0))
        (p1 (vec3 1.0 1.0 1.0))
        (p2 (vec3 0.0 0.0 0.0)))
    
    (format t "~a . ~a = ~a~%" v1 v2 (v. v1 v2))
    (format t "~a x ~a = ~a~%" v1 v2 (vc v1 v2))
    (format t "~a + ~a = ~a~%" v1 v2 (v+ v1 v2))
    (format t "~a - ~a = ~a~%" v1 v2 (v- v1 v2))
    (format t "~a - ~a = ~a~%" p1 p2 (v- p1 p2))
    (format t "~a normalized = ~a~%" v3 (vunit v3))
    ))

(defun test-trace (fname)
  (let ((scn (make-scene :eye-point (vec3 16.0 16.0 16.0))))
    (add-light scn (make-light :location (vec3 40.0 40.0 40.0)))
    (add-object scn (make-sphere :radius 4.0
                                 :location (vec3 8.0 0.0 0.0)
                                 :material (make-material
                                            :kd (vec3 0.0 1.0 0.0))))
    (add-object scn (make-sphere :radius 4.0))
    (rtrace:rtrace scn fname)))
          
(defun test-stl (fname stl)
  (let ((scn (make-scene :eye-point (vec3 3.5 5.0 4.0))))
    (add-light scn (make-light :location (vec3 80.0 80.0 80.0)))
    (add-light scn (make-light :location (vec3 80.0 80.0 -80.0)))
    (add-light scn (make-light :location (vec3 -80.0 80.0 -80.0)))
    (add-light scn (make-light :location (vec3 -80.0 80.0 80.0)))
    (add-object scn (make-rstl :tris stl))
    (Rtrace:rtrace scn fname)))
