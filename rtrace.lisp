;;;; rtrace.lisp

(in-package #:rtrace)

;; (declaim (optimize (speed 3) (safety 1) (debug 0)))


(declaim (inline itrace shade compute-eye-ray set-pixel point-on-ray)
         (ftype (function (point point rvector double-float fixnum fixnum fixnum fixnum) ray) compute-eye-ray)
         (ftype (function ((simple-array (unsigned-byte 8) (* * *)) fixnum fixnum color)) set-pixel)
         (ftype (function (scene rintersection) color) shade)
         (ftype (function (scene ray) color) itrace)
         (ftype (function (ray double-float) point) point-on-ray))

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
  (declare (type ray r)
           (type double-float tv))
  (pvadd (ray-origin r) (vscale tv (ray-direction r))))

(defgeneric intersects (r obj)
  (:documentation "Compute an intersection for a ray and object."))

(defmethod intersects ((r ray) (s sphere))
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
             (type double-float under))
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
          :normal (vnormalize (psub (sphere-location s) pt)))))
      (t nil))))
       ;; (make-rintersection :tval 0.0 :intersects nil
       ;;                     :object s :ray r )))))

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
  (let* ((pdiff (vnormalize (psub origin look-at)))
         (right (cross pdiff up))
         (rc (vscale (* aspect (+ -0.5 (/ (coerce j 'double-float) (coerce width 'double-float)  1.0))) right))
         (uc (vscale (+ -0.5 (/ (coerce i 'double-float) (coerce height 'double-float) 1.0)) up))
         (vdir (vadd pdiff (vadd  rc uc))))
    (declare (type rvector pdiff right rc uc vdir))
    (make-ray :origin origin
              :direction (if (= 0.0 (vlen vdir))
                             vdir
                             (vnormalize vdir)))))

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
                    (nldir (vnormalize ldir))
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
        (declare (type (or null rintersection) isect))
        (if (and isect (rintersection-intersects isect) (< (rintersection-tval isect) (rintersection-tval nearest)))
            (setf nearest isect))))
    (the color (shade scn nearest))))

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
    (format t "~a normalized = ~a~%" v3 (vnormalize v3))
    ))

(defun test-trace (fname)
  (ensure-directories-exist fname)
  (let ((scn (make-scene :eye-point (make-point 0.0 0.0 20.0))))
    (add-light scn (make-light :location (make-point 40.0 40.0 40.0)))
    (add-object scn (make-sphere :radius 4.0 :location (make-point 0.0 0.0 0.0) :material (make-material :kd (make-rgb 0.8 0.9 0.1))))
    (add-object scn (make-sphere :radius 2.0 :location (make-point 0.0 6.0 -8.0) :material (make-material :kd (make-rgb 0.8 0.1 0.9))))
    (rtrace:rtrace scn fname)
    ))

(defun test-animation (directory duration)
  (ensure-directories-exist directory)
  (let* ((frame-count (* duration 30))
         (dt (/ (* 2 pi) frame-count)))
    (dotimes (i frame-count)
      (let ((scn (make-scene :eye-point (make-point 0.0 0.0 20.0)))
            (tv (* dt i))
            (fname (format nil "~a/frame~5,'0d.png" directory i)))
        (add-light scn (make-light :location (make-point 40.0 40.0 40.0)))
        (add-object scn (make-sphere :radius 10.0 :location (make-point 0.0 0.0 0.0) :material (make-material :kd (make-rgb 0.8 0.9 0.1))))
        (add-object scn (make-sphere :radius 1.0 :location (make-point (* 12.0 (cos tv)) 0.0 (* 12.0 (sin tv))) :material (make-material :kd (make-rgb 0.1 0.9 0.1))))
        (rtrace:rtrace scn fname)))))
          
