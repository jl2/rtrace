;;;; rtrace.lisp

(in-package #:rtrace)

(defstruct point
  (x 0.0 :type single-float)
  (y 0.0 :type single-float)
  (z 0.0 :type single-float))

(defstruct rvector
  (x 0.0 :type single-float)
  (y 0.0 :type single-float)
  (z 0.0 :type single-float))

(declaim (optimize (speed 3) (safety 1) (debug 2)))

(defmacro each-point-to-vector (op p1 p2)
  `(make-rvector :x (funcall ,op (point-x ,p1) (point-x ,p2))
                 :y (funcall ,op (point-y ,p1) (point-y ,p2))
                 :z (funcall ,op (point-z ,p1) (point-z ,p2))))

(defmacro each-vector-to-vector (op p1 p2)
  `(make-rvector :x (funcall ,op (rvector-x ,p1) (rvector-x ,p2))
                 :y (funcall ,op (rvector-y ,p1) (rvector-y ,p2))
                 :z (funcall ,op (rvector-z ,p1) (rvector-z ,p2))))

(defun vscale (k v)
  (declare (type single-float k)
           (type rvector v))
  (make-rvector :x (* k (rvector-x v))
                :y (* k (rvector-y v))
                :z (* k (rvector-z v))))

(defun psub (p1 p2)
  (declare (type point p1 p2))
  (each-point-to-vector #'- p1 p2))

(defun vsub (v1 v2)
  (declare (type rvector v1 v2))
  (each-vector-to-vector #'- v1 v2))

(defun vadd (v1 v2)
  (declare (type rvector v1 v2))
  (each-vector-to-vector #'+ v1 v2))

(defun pvadd (p v)
  (declare (type point p)
           (type rvector v))
  (make-point :x (+ (point-x p) (rvector-x v))
              :y (+ (point-y p) (rvector-y v))
              :z (+ (point-z p) (rvector-z v))))

(defun dot (v1 v2)
  (declare (type rvector v1 v2))
  (+ (* (rvector-x v1) (rvector-x v2))
     (* (rvector-y v1) (rvector-y v2))
     (* (rvector-z v1) (rvector-z v2))))

(defun cross (v1 v2)
  (declare (type rvector v1 v2))
  (let ((v1x (rvector-x v1))
        (v1y (rvector-y v1))
        (v1z (rvector-z v1))
        (v2x (rvector-x v2))
        (v2y (rvector-y v2))
        (v2z (rvector-z v2)))
    (make-rvector :x (- (* v1y v2z) (* v1z v2y))
                 :y (- (* v1z v2x) (* v1x v2z))
                 :z (- (* v1x v2y) (* v1y v2x)))))

(defun vlength (vect)
  (declare (type rvector vect))
  (let ((xl (rvector-x vect))
        (yl (rvector-y vect))
        (zl (rvector-z vect)))
    (sqrt (+ (* xl xl) (* yl yl) (* zl zl)))))

(defun normalize (vect)
  (declare (type rvector vect))
  (let ((ilen (/ 1.0 (vlength vect))))
    (vscale ilen vect)))

(defstruct ray
  (origin (make-point) :type point)
  (direction (make-rvector) :type rvector))

(defstruct rgb-color
  (r 1.0 :type single-float)
  (g 1.0 :type single-float)
  (b 1.0 :type single-float))

(defmacro each-color-to-color (op c1 c2)
  `(make-rgb-color :r (funcall ,op (rgb-color-r ,c1) (rgb-color-r ,c2))
                   :g (funcall ,op (rgb-color-g ,c1) (rgb-color-g ,c2))
                   :b (funcall ,op (rgb-color-b ,c1) (rgb-color-b ,c2))))

(defun cadd (c1 c2)
  (declare (type rgb-color c1 c2))
  (each-color-to-color #'+ c1 c2))

(defun cscale (k c)
  (declare (type single-float k)
           (type rgb-color c))
  (make-rgb-color :r (* k (rgb-color-r c))
                  :g (* k (rgb-color-g c))
                  :b (* k (rgb-color-b c))))

(defun cmul (c1 c2)
  (declare (type rgb-color c1 c2))
  (each-color-to-color #'* c1 c2))

(defun csub (c1 c2)
  (declare (type rgb-color c1 c2))
  (each-color-to-color #'- c1 c2))

(defstruct material
  (kd (make-rgb-color) :type rgb-color)
  (ks (make-rgb-color) :type rgb-color)
  (ka (make-rgb-color :r 0.1 :g 0.1 :b 0.1) :type rgb-color)
  (kr (make-rgb-color :r 0.0 :g 0.0 :b 0.0) :type rgb-color)
  (kt (make-rgb-color :r 0.0 :g 0.0 :b 0.0) :type rgb-color)
  (ior 1.0 :type single-float))

(defstruct sphere
  (radius 1.0 :type single-float)
  (location (make-point) :type point)
  (material (make-material) :type material))

(defgeneric material (obj)
  (:documentation "Return an object's material."))

(defmethod material ((obj sphere))
  (sphere-material obj))

(defstruct rintersection
  (tval 999999999.0 :type single-float)
  (intersects nil :type boolean)
  (object nil)
  (ray nil)
  (point nil)
  (normal nil))

(defun point-on-ray (r tv)
  (pvadd (ray-origin r) (vscale tv (ray-direction r))))

(defgeneric intersects (r obj)
  (:documentation "Compute an intersection for a ray and object."))

(defmethod intersects (r (s sphere))
  (let* ((oc (psub (ray-origin r) (sphere-location s)))
         (rad (sphere-radius s))
         (lenoc (vlength oc))
         (ldoc (dot (ray-direction r) oc))
         (lenl (vlength (ray-direction r)))
         (under  (- (* ldoc ldoc) (* (* lenl lenl) (- (* lenoc lenoc) (* rad rad))))))
    (declare (type rvector oc)
             (type single-float rad lenoc ldoc lenl under))
    (cond 
          ((= under 0.0)
           (let* ((tv (the single-float (/ 
                                         (the single-float (+ (the single-float (- ldoc)) (the single-float (sqrt (the single-float under)))))
                                         (the single-float (* lenl lenl)))))
                 (pt (point-on-ray r tv)))
             (declare (type point pt)
                      (type single-float tv))
             (make-rintersection
              :tval tv
              :intersects t
              :object s :ray r
              :normal (normalize (psub (sphere-location s) pt))
              :point pt)))

          ((> under 0.0)
           (let* ((tv (/ (min (+ (- ldoc) (sqrt under))
                              (- (- ldoc) (sqrt under))) (* lenl lenl)))
                  (pt (point-on-ray r tv)))
             (declare (type point pt)
                      (type single-float tv))
             (make-rintersection
              :tval tv
              :intersects t
              :object s :ray r
              :point pt
              :normal (normalize (psub (sphere-location s) pt)))))
          (t
           (make-rintersection :tval 0.0 :intersects nil
                               :object s :ray r )))))

(defstruct light
  (location (make-point) :type point)
  (color (make-rgb-color) :type rgb-color))

(defstruct scene
  (eye-point (make-point :x 10.0f0 :y 10.0f0 :z 10.0f0) :type point)
  (look-at (make-point) :type point)
  (up (make-rvector :x 0.0f0 :y 1.0f0 :z 0.0f0))
  (aspect (/ 4.0 3.0) :type single-float)
  (objects nil :type list)
  (lights nil :type list))

(defun create-scene ()
  (make-scene))

(defun add-object (scn sph)
  (setf (scene-objects scn) (cons sph (scene-objects scn))))

(defun add-light (scn lght)
  (setf (scene-lights scn) (cons lght (scene-lights scn))))

(defun compute-eye-ray (origin look-at up aspect x y width height)
  (declare (type point origin look-at)
           (type rvector up)
           (type (signed-byte 64) x y width height)
           (type single-float aspect))
  (let* ((pdiff (normalize (psub origin look-at)))
         (right (cross up pdiff))
         (rc (vscale (* aspect (+ -0.5f0 (/ x 1.0f0 width))) right))
         (uc (vscale (+ -0.5 (/ y 1.0f0 height)) up))
         (vdir (vadd pdiff (vadd  rc uc))))
    (declare (type rvector pdiff right rc uc vdir))
    (make-ray :origin origin
              :direction (if (= 0.0f0 (the single-float (vlength vdir)))
                             vdir
                             (normalize vdir)))))

(defun set-pixel (img x y rgb)
  (let ((r (truncate (the single-float (* 255.0f0 (rgb-color-r rgb)))))
        (g (truncate (the single-float (* 255.0f0 (rgb-color-g rgb)))))
        (b (truncate (the single-float (* 255.0f0 (rgb-color-b rgb))))))
    (declare (type fixnum r g b)
             (type (simple-array  (unsigned-byte 8) *) img)
             (type fixnum x y))
    (setf (aref img y x 0) r)
    (setf (aref img y x 1) g)
    (setf (aref img y x 2) b)))

(defun clamp (val &key (max 1.0f0) (min 0.0f0))
  (declare (type single-float val max min))
  (cond ((< val min) min)
        ((> val max) max)
        (t val)))

(defun shade (scn isect)
  (declare (type scene scn)
           (type rintersection isect))
  (let ((ry (rintersection-ray isect)))
    (declare (type ray ry)
             (ignorable ry))
    (if (rintersection-intersects isect)
        (let ((rval (make-rgb-color :r 0.0f0 :g 0.0f0 :b 0.0f0)))
          (declare (type rgb-color rval))
          (dolist (lght (scene-lights scn))
            (declare (type light lght))
            (let*  ((ldir (psub (light-location lght) (rintersection-point isect)))
                    ;; (ldist (vlength ldir))
                    (nldir (normalize ldir))
                    (ndl (dot nldir (rintersection-normal isect)))
                    (intensity (clamp ndl)))
              (declare (type rvector ldir nldir)
                       (type single-float ndl intensity))
              (setf rval (cadd rval (cscale intensity (cmul (light-color lght) (material-kd (material (rintersection-object isect)))))))))
          rval)
        
        (make-rgb-color :r 0.0f0
                        :g 0.0f0
                        :b 0.0f0))))

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
  (let ((v1 (make-rvector :x 1.0f0 :y 0.0f0 :z 0.0f0))
        (v2 (make-rvector :x 0.0f0 :y 1.0f0 :z 0.0f0))
        (v3 (make-rvector :x 10.0f0 :y 10.0f0 :z 10.0f0))
        (p1 (make-point :x 1.0f0 :y 1.0f0 :z 1.0f0))
        (p2 (make-point :x 0.0f0 :y 0.0f0 :z 0.0f0)))
    
    (format t "~a . ~a = ~a~%" v1 v2 (dot v1 v2))
    (format t "~a x ~a = ~a~%" v1 v2 (cross v1 v2))
    (format t "~a + ~a = ~a~%" v1 v2 (vadd v1 v2))
    (format t "~a - ~a = ~a~%" v1 v2 (vsub v1 v2))
    (format t "~a - ~a = ~a~%" p1 p2 (psub p1 p2))
    (format t "~a normalized = ~a~%" v3 (normalize v3))
    ))

(defun test-trace (fname)
  (let ((scn (make-scene :eye-point (make-point :x 16.0f0 :y 16.0f0 :z 16.0f0))))
    (add-light scn (make-light :location (make-point :x 40.0 :y 40.0 :z 40.0)))
    (add-object scn (make-sphere :radius 4.0 :location (make-point :x 8.0)))
    (add-object scn (make-sphere :radius 4.0))
    (rtrace:rtrace scn fname)))
          
