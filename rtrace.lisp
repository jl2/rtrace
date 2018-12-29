;;;; rtrace.lisp

(in-package #:rtrace)

;; (declaim (optimize (speed 3) (safety 1) (debug 0)))
(declaim (optimize speed))
(deftype point ()
  '(simple-array double-float (3)))

(deftype rvector ()
  '(simple-array double-float (4)))

(deftype color ()
  '(simple-array double-float (4)))

(declaim (inline
          make-point
          make-rvector
          px py pz
          vx vy vz
          (setf px) (setf py) (setf pz)
          (setf vx) (setf vy) (setf vz)
          psub pvadd
          vsub vadd dot cross vscale
          vlen vlen-squared
          normalize
          make-rgb
          make-rgba
          cadd csub cmul cscale
          cclamp
          )
         
         (ftype (function (point) double-float) px py pz)
         (ftype (function (rvector) double-float) vx vy vz)
         (ftype (function (point double-float) double-float) (setf px) (setf py) (setf pz))
         (ftype (function (rvector double-float) double-float) (setf vx) (setf vy) (setf vz))

         (ftype (function (double-float double-float double-float) point) make-point)
         (ftype (function (double-float double-float double-float) rvector) make-rvector)
         (ftype (function (double-float double-float double-float double-float) rvector) make-rvector-4)
         (ftype (function (point point) rvector) psub)
         (ftype (function (point rvector) point) pvadd)
         (ftype (function (rvector rvector) rvector) vadd vsub cross)
         (ftype (function (rvector rvector) double-float) dot)
         (ftype (function (double-float rvector) rvector) vscale)
         (ftype (function (rvector) (double-float 0.0 *)) vlen vlen-squared)
         (ftype (function (rvector) rvector) normalize)

         (ftype (function (color color) color) cadd csub cmul)
         (ftype (function (color) color) cclamp)
         (ftype (function (double-float color) color) cscale))

(defun px (v)
  (declare (type point v))
  (aref v 0))
(defun (setf px) (v value)
  (declare (type point v)
           (type double-float value))
  (setf (aref v 0) value))

(defun py (v)
  (declare (type point v))
  (aref v 1))
(defun (setf py) (v value)
  (declare (type point v)
           (type double-float value))
  (setf (aref v 1) value))

(defun pz (v)
  (declare (type point v))
  (aref v 2))
(defun (setf pz) (v value)
  (declare (type point v)
           (type double-float value))
  (setf (aref v 2) value))

(defun make-point (x y z)
  (declare (type double-float x y z))
  (let ((rval (make-array '(3) :element-type 'double-float :initial-element 1.0d0)))
    (setf (aref rval 0) x)
    (setf (aref rval 1) y)
    (setf (aref rval 2) z)
    rval))

(defun make-rvector (x y z)
  (declare (type double-float x y z))
  (let ((rval (make-array '(4) :element-type 'double-float :initial-element 1.0d0)))
    (setf (aref rval 0) x)
    (setf (aref rval 1) y)
    (setf (aref rval 2) z)
    rval))

(defun make-rvector-4 (x y z w)
  (declare (type double-float x y z w))
  (let ((rval (make-array '(4) :element-type 'double-float :initial-element 0.0d0)))
    (setf (aref rval 0) x)
    (setf (aref rval 1) y)
    (setf (aref rval 2) z)
    (setf (aref rval 3) w)
    rval))
  



(defun vx (v)
  (declare (type rvector v))
  (aref v 0))
(defun (setf vx) (v value)
  (declare (type rvector v)
           (type double-float value))
  (setf (aref v 0) value))

(defun vy (v)
  (declare (type rvector v))
  (aref v 1))
(defun (setf vy) (v value)
  (declare (type rvector v)
           (type double-float value))
  (setf (aref v 1) value))

(defun vz (v)
  (declare (type rvector v))
  (aref v 2))
(defun (setf vz) (v value)
  (declare (type rvector v)
           (type double-float value))
  (setf (aref v 2) value))

(defun vw (v)
  (declare (type rvector v))
  (aref v 3))
(defun (setf vw) (v value)
  (declare (type rvector v)
           (type double-float value))
  (setf (aref v 3) value))

(defun vscale (k v)
  (declare (type double-float k)
           (type rvector v))
  (make-rvector (* k (vx v))
                (* k (vy v))
                (* k (vz v))))

(defun psub (p1 p2)
  (declare (type point p1 p2))
  (make-rvector (- (px p1) (px p2))
                (- (py p1) (py p2))
                (- (pz p1) (pz p2))))

(defun vsub (v1 v2)
  (declare (type rvector v1 v2))
  (make-rvector (- (vx v1) (vx v2))
                (- (vy v1) (vy v2))
                (- (vz v1) (vz v2))))

(defun vadd (v1 v2)
  (declare (type rvector v1 v2))
  (make-rvector (+ (vx v1) (vx v2))
                (+ (vy v1) (vy v2))
                (+ (vz v1) (vz v2))))

(defun pvadd (p v)
  (declare (type point p)
           (type rvector v))
  (make-point (+ (px p) (vx v))
                (+ (py p) (vy v))
                (+ (pz p) (vz v))))

(defun dot (v1 v2)
  (declare (type rvector v1 v2))
  (+ (* (vx v1) (vx v2))
     (* (vy v1) (vy v2))
     (* (vz v1) (vz v2))))

(defun cross (v1 v2)
  (declare (type rvector v1 v2))
  (let ((v1x (vx v1))
        (v1y (vy v1))
        (v1z (vz v1))
        (v2x (vx v2))
        (v2y (vy v2))
        (v2z (vz v2)))
    (make-rvector (- (* v1y v2z) (* v1z v2y))
                  (- (* v1z v2x) (* v1x v2z))
                  (- (* v1x v2y) (* v1y v2x)))))

(defun vlen (vect)
  (declare (type rvector vect))
  (let ((xl (vx vect))
        (yl (vy vect))
        (zl (vz vect)))
    (the (double-float 0.0) (sqrt (the (double-float 0.0 *) (+ (* xl xl) (* yl yl) (* zl zl)))))))

(defun vlen-squared (vect)
  (declare (type rvector vect))
  (let ((xl (vx vect))
        (yl (vy vect))
        (zl (vz vect)))
    (+ (* xl xl) (* yl yl) (* zl zl))))

(defun normalize (vect)
  (declare (type rvector vect))
  (let ((ilen (/ 1.0 (vlen vect))))
    (vscale ilen vect)))

(defun make-rgb (r g b)
  (declare (type double-float r g b))
  (let ((rval (make-array '(4) :element-type 'double-float :initial-element 0.0)))
    (setf (aref rval 0) r)
    (setf (aref rval 1) g)
    (setf (aref rval 2) b)
    (setf (aref rval 3) 1.0)
    rval))

(defun make-rgba (r g b a)
  (declare (type double-float r g b a))
  (let ((rval (make-array '(4) :element-type 'double-float :initial-element 0.0)))
    (setf (aref rval 0) r)
    (setf (aref rval 1) g)
    (setf (aref rval 2) b)
    (setf (aref rval 3) a)
    rval))

(defun red (color)
  (declare (type color color))
  (aref color 0))
(defun (setf red) (color value)
  (declare (type color color)
           (type double-float value))
  (setf (aref color 0) value))

(defun green (color)
  (declare (type color color))
  (aref color 1))
(defun (setf green) (color value)
  (declare (type color color)
           (type double-float value))
  (setf (aref color 1) value))

(defun blue (color)
  (declare (type color color))
  (aref color 2))
(defun (setf blue) (color value)
  (declare (type color color)
           (type double-float value))
  (setf (aref color 2) value))

(defun alpha (color)
  (declare (type color color))
  (aref color 3))
(defun (setf alpha) (color value)
  (declare (type color color)
           (type double-float value))
  (setf (aref color 3) value))

(defun cscale (k c)
  (declare (type double-float k)
           (type color c))
  (make-rgba (* k (red c))
             (* k (green c))
             (* k (blue c))
             (* k (alpha c))))

(defun cadd (c1 c2)
  (declare (type color c1 c2))
  (make-rgba (+ (red c1) (red c2))
             (+ (green c1) (green c2))
             (+ (blue c1) (blue c2))
             (+ (alpha c1) (alpha c2))))

(defun cmul (c1 c2)
  (declare (type color c1 c2))
  (make-rgba (* (red c1) (red c2))
             (* (green c1) (green c2))
             (* (blue c1) (blue c2))
             (* (alpha c1) (alpha c2))))

(defun csub (c1 c2)
  (declare (type color c1 c2))
  (make-rgba (- (red c1) (red c2))
             (- (green c1) (green c2))
             (- (blue c1) (blue c2))
             (- (alpha c1) (alpha c2))))


(declaim (inline 
          ishade
          compute-eye-ray
          )
         
         (ftype (function (point point rvector double-float fixnum fixnum fixnum fixnum) ray) compute-eye-ray)
         (ftype (function ((simple-array (unsigned-byte 8) (* * *)) fixnum fixnum color)) set-pixel)
         (ftype (function (scene rintersection) color) shade)
         (ftype (function (scene ray) color) itrace)
         )

(defstruct light
  (location (make-point 0.0 0.0 0.0) :type point)
  (color (make-rgb 1.0 1.0 1.0) :type color))

(defstruct scene
  (eye-point (make-point 10.0 10.0 10.0) :type point)
  (look-at (make-point 0.0 0.0 0.0) :type point)
  (up (make-rvector 0.0 1.0 0.0))
  (aspect (/ 4.0 3.0) :type double-float)
  (objects nil :type list)
  (lights nil :type list))

(defstruct ray
  (origin (make-point 0.0 0.0 0.0) :type point)
  (direction (make-rvector 1.0 0.0 0.0) :type rvector))


(defstruct material
  (kd (make-rgba 0.1 0.8 0.1 1.0) :type color)
  (ks (make-rgba 1.0 1.0 1.0 1.0) :type color)
  (ka (make-rgba 0.1 0.1 0.1 1.0) :type color)
  (kr (make-rgba 0.0 0.0 0.0 1.0) :type color)
  (kt (make-rgba 0.0 0.0 0.0 1.0) :type color)
  (ior 1.0 :type double-float))

(defstruct sphere
  (radius 1.0 :type double-float)
  (location (make-point 0.0 0.0 0.0) :type point)
  (material (make-material) :type material))

(defstruct rintersection
  (tval 999999999.0 :type double-float)
  (intersects nil :type boolean)
  (object nil)
  (ray nil)
  (point nil)
  (normal nil))



(defgeneric material (obj)
  (:documentation "Return an object's material."))

(defmethod material ((obj sphere))
  (sphere-material obj))


(defun point-on-ray (r tv)
  (pvadd (ray-origin r) (vscale tv (ray-direction r))))

(defgeneric intersects (r obj)
  (:documentation "Compute an intersection for a ray and object."))

(defmethod intersects (r (s sphere))
  (declare (type ray r)
           (type sphere s))
  (let* ((oc (psub (ray-origin r) (sphere-location s)))
         (rad (sphere-radius s))
         (lenoc (vlen oc))
         (ldoc (dot (ray-direction r) oc))
         (lenl (vlen (ray-direction r)))
         (under  (- (* ldoc ldoc) (* (* lenl lenl) (- (* lenoc lenoc) (* rad rad))))))
    (declare (type rvector oc)
             (type double-float rad lenoc ldoc lenl)
             (type (double-float 0.0 *) under))
    
    (cond 
      ((> under 0.0)
       (let* ((tv (/ (min (+ (- ldoc) (sqrt under))
                          (- (- ldoc) (sqrt under)))
                     (* lenl lenl)))
              (pt (point-on-ray r tv)))
         (declare (type point pt)
                  (type double-float tv))
         (make-rintersection
          :tval tv
          :intersects t
          :object s :ray r
          :point pt
          :normal (normalize (psub (sphere-location s) pt)))))
      (t
       (make-rintersection :tval 0.0 :intersects nil
                           :object s :ray r )))))

(defun create-scene ()
  (make-scene))

(defun add-object (scn obj)
  (declare (type scene scn)
           (type t obj))
  (setf (scene-objects scn) (cons obj (scene-objects scn))))

(defun add-light (scn lght)
  (declare (type scene scn)
           (type light lght))
  (setf (scene-lights scn) (cons lght (scene-lights scn))))


(defun compute-eye-ray (origin look-at up aspect i j width height)
  (declare (type point origin look-at)
           (type rvector up)
           (type double-float aspect)
           (type fixnum i j width height))
  (let* ((pdiff (normalize (psub origin look-at)))
         (right (cross up pdiff))
         (rc (vscale (* aspect (+ -0.5 (/ (coerce i 'double-float) (coerce width 'double-float)  1.0))) right))
         (uc (vscale (+ -0.5 (/ (coerce j 'double-float) (coerce height 'double-float) 1.0)) up))
         (vdir (vadd pdiff (vadd  rc uc))))
    (declare (type rvector pdiff right rc uc vdir))
    (make-ray :origin origin
              :direction (if (= 0.0 (vlen vdir))
                             vdir
                             (normalize vdir)))))

(defun set-pixel (img i j rgba)
  (declare (type fixnum i j)
           (type color rgba)
           (type (simple-array (unsigned-byte 8) (* * *)) img))
  (let ((r (* 255 (red rgba)))
        (g (* 255 (green rgba)))
        (b (* 255 (blue rgba)))
        (a (* 255 (alpha rgba))))
    (setf (aref img j i 0) (truncate r))
    (setf (aref img j i 1) (truncate g))
    (setf (aref img j i 2) (truncate b))
    (setf (aref img j i 3) (truncate a)))
  (values))

(defun cclamp (color)
  (declare (type color color))
  (make-rgba (alexandria:clamp (red color) 0.0 1.0)
             (alexandria:clamp (green color) 0.0 1.0)
             (alexandria:clamp (blue color) 0.0 1.0)
             (alexandria:clamp (alpha color) 0.0 1.0)))

(defun shade (scn isect)
  (declare (type scene scn)
           (type rintersection isect))
    (if (rintersection-intersects isect)
        (let ((rval (make-rgb 0.0 0.0 0.0)))
          (dolist (light (scene-lights scn))
            (let*  ((ldir (psub (light-location light) (rintersection-point isect)))
                    (nldir (normalize ldir))
                    (ndl (dot nldir (rintersection-normal isect)))
                    (intensity (alexandria:clamp ndl 0.0 1.0)))
              (declare (type double-float ndl intensity)
                       (type rvector ldir nldir))
              (setf rval (cadd rval (cscale intensity (cmul (light-color light) (material-kd (material (rintersection-object isect)))))))))
          rval)
        (make-rgb 0.0 0.0 0.0)))

(defun itrace (scn ry)
  (declare (type scene scn)
           (type ray ry))
  (let ((nearest (make-rintersection :ray ry)))
    (declare (type rintersection nearest))
    (dolist (sph (scene-objects scn))
      (let ((isect (intersects ry sph)))
        (declare (type rintersection isect))
        (if (and (rintersection-intersects isect) (< (rintersection-tval isect) (rintersection-tval nearest)))
            (setf nearest isect))))
    (the rgb-color (shade scn nearest))))

(defun rtrace (scn file-name &key (width 800) (height 600))
  (declare (type fixnum width height)
           (type (or pathname string) file-name)
           (type scene scn))
  (let ((img (png:make-image height width 4 8)))
    (declare (type (simple-array (unsigned-byte 8) (* * *)) img))
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
          (set-pixel img i j (cclamp (itrace scn r))))))
    (with-open-file (output file-name :element-type '(unsigned-byte 8) :direction :output :if-exists :supersede)
                    (png:encode img output))))

(defun run-tests ()
  (let ((v1 (make-rvector 1.0 0.0 0.0))
        (v2 (make-rvector 0.0 1.0 0.0))
        (v3 (make-rvector 10.0 10.0 10.0))
        (p1 (make-point 1.0 1.0 1.0))
        (p2 (make-point 0.0 0.0 0.0)))
    (format t "~a . ~a = ~a~%" v1 v2 (dot v1 v2))
    (format t "~a x ~a = ~a~%" v1 v2 (cross v1 v2))
    (format t "~a + ~a = ~a~%" v1 v2 (vadd v1 v2))
    (format t "~a - ~a = ~a~%" v1 v2 (vsub v1 v2))
    (format t "~a - ~a = ~a~%" p1 p2 (psub p1 p2))
    (format t "~a normalized = ~a~%" v3 (normalize v3))
    ))

(defun test-trace (fname)
  (ensure-directories-exist fname)
  (let ((scn (make-scene :eye-point (make-point 16.0 16.0 16.0))))
    (add-light scn (make-light :location (make-point 40.0 40.0 40.0)))
    (add-object scn (make-sphere :radius 4.0 :location (make-point 8.0 0.0 0.0) :material (make-material :kd (make-rgb 0.1 0.9 0.1))))
    (add-object scn (make-sphere :radius 4.0))
    (rtrace:rtrace scn fname)
    ))
          
