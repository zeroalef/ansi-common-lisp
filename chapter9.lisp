(defun square (n) (* n n))

(defun mag (x y z)
  (sqrt (apply #'+ (mapcar #'square `(,x ,y ,z)))))

(defun unit-vector (x y z)
  (let ((d (mag x y z)))
    (values (/ x d) (/ y d) (/ z d))))

(defstruct (point)
  x y z)

(defun distance (p1 p2)
  (mag (- (point-x p1) (point-x p2))
       (- (point-y p1) (point-y p2))
       (- (point-z p1) (point-z p2))))

(defun minroot (a b c)
  (if (zerop a)
      (/ (- c) b)
      (let ((disc (- (square b) (* 4 a c))))
        (unless (minusp disc)
          (let ((discrt (sqrt disc)))
            (min (/ (+ (- b) discrt) (* a 2))
                 (/ (- (- b) discrt) (* a 2))))))))

(defstruct surface color)
(defstruct (sphere
            (:include surface))
  radius center)

(defparameter *world* nil)
(defconstant +eye+ (make-point :x 0 :y 0 :z 0))

(defun tracer (pathname &optional (res 1))
  (with-open-file (stream pathname
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :overwrite)
    (format stream "P2 ~A ~A 255" (* res 100) (* res 100))
    (let ((inc (/ res)))
      (do ((y -50 (+ y inc)))
          ((< (- 50 y) inc))
        (do ((x -50 (+ x inc)))
            ((< (- 50 x) inc))
          (print (color-at x y) stream))))))

(defun color-at (x y)
  (multiple-value-bind (xr yr zr)
      (unit-vector (- x (point-x +eye+))
                   (- y (point-y +eye+))
                   (- 0 (point-z +eye+)))
    (round (* (sendray +eye+ xr yr zr) 255))))

(defun sendray (pt xr yr zr)
  (multiple-value-bind (s int) (first-hit pt xr yr zr)
    (if s
        (* (lambert s int xr yr zr) (surface-color s))
        0)))

(defun first-hit (pt xr yr zr)
  (let (surface hit dist)
    (dolist (s *world*)
      (let ((h (intersect s pt xr yr zr)))
        (when h
          (let ((d (distance h pt)))
            (when (or (null dist) (< d dist))
              (setf surface s hit h dist d))))))
    (values surface hit)))

(defun lambert (s int xr yr zr)
  (multiple-value-bind (xn yn zn) (normal s int)
    (max 0 (+ (* xn xr) (* yn yr) (* zn zr)))))

(defun defsphere (x y z r c)
  (let ((s (make-sphere
            :radius r
            :center (make-point :x x :y y :z z)
            :color c)))
    (push s *world*)
    s))

(defun intersect (s pt xr yr zr)
  (funcall (typecase s (sphere #'sphere-intersect))
           s pt xr yr zr))

(defun sphere-intersect (s pt xr yr zr)
  (let* ((c (sphere-center s))
         (n (minroot (+ (square xr) (square yr) (square zr))
                     (* 2 (+ (* (- (point-x pt) (point-x c)) xr)
                             (* (- (point-y pt) (point-y c)) yr)
                             (* (- (point-z pt) (point-z c)) zr)))
                     (+ (square (- (point-x pt) (point-x c)))
                        (square (- (point-y pt) (point-y c)))
                        (square (- (point-z pt) (point-z c)))
                        (- (square (sphere-radius s)))))))
    (when n
      (make-point :x (+ (point-x pt) (* n xr))
                  :y (+ (point-y pt) (* n yr))
                  :z (+ (point-z pt) (* n zr))))))

(defun normal (s pt)
  (funcall (typecase s (sphere #'sphere-normal))
           s pt))

(defun sphere-normal (s pt)
  (let ((c (sphere-center s)))
    (unit-vector (- (point-x c) (point-x pt))
                 (- (point-y c) (point-y pt))
                 (- (point-z c) (point-z pt)))))

(defun ray-test (&optional (res 1))
  (setf *world* nil)
  (defsphere 0 -300 -1200 200 .8)
  (defsphere -80 -150 -1200 200 .7)
  (defsphere 70 -100 -1200 200 .9)
  (do ((x 1 (1+ x)))
      ((> x 4))
    (do ((z 2 (1+ z)))
        ((> z 7))
      (defsphere (* x 200) 300 (* z -400) 40 .75)))
  (tracer (make-pathname :name "spheres.pgm") res))


;;; task 1
(defun non-decreasingp (lst)
  (and (listp lst)
       (numberp (car lst))
       (reduce #'(lambda (a b)
                   (if (and a (numberp b) (<= a b)) b nil))
               lst)))

;;; task 2
(defun coins (n &optional (nominal '(25 10 5)) acc)
  (if (null nominal)
      (reverse (cons (cons n 1) acc))
      (let ((tmp (car nominal)))
        (multiple-value-bind (counter remainder) (floor n tmp)
          (coins remainder
                 (cdr nominal)
                 (if (plusp counter)
                     (cons (cons counter tmp)
                           acc)
                     acc))))))

;;; task 3
(defun championship (&optional (candidates 10))
  (when (oddp candidates)
    (format t "the candidates amount if not even: ~A" candidates)
    (return-from championship nil))
  (do ((i 0 (1+ i))
       (wiggles 0)
       (wobbles 0)
       (half (/ candidates 2)))
      ((>= i candidates)
       (format t "wiggles: ~A~%wobbles: ~A~%" wiggles wobbles))
    (if (> half (random candidates))
        (incf wiggles)
        (incf wobbles))))


;;; task 4.a
(defstruct (2d-vector (:include point))
  start end)

(defun defvector (x1 y1 x2 y2)
  (make-2d-vector
   :start (make-point :x x1 :y y1 :z nil )
   :end (make-point :x x2 :y y2 :z nil)))

(defun 2d-vector-intersect (v1 v2)
  (let* ((s1x (- (point-x (2d-vector-end v1))
                 (point-x (2d-vector-start v1))))
         (s1y (- (point-y (2d-vector-end v1))
                 (point-y (2d-vector-start v1))))
         (s2x (- (point-x (2d-vector-end v2))
                 (point-x (2d-vector-start v2))))
         (s2y (- (point-y (2d-vector-end v2))
                 (point-y (2d-vector-start v2))))
         (s (/ (+ (* (- s1y)
                     (- (point-x (2d-vector-start v1))
                        (point-x (2d-vector-start v2))))
                  (* s1x
                     (- (point-y (2d-vector-start v1))
                        (point-y (2d-vector-start v2)))))
               (+ (* (- s2x) s1y)
                  (* s1x s2y))))
         (tt (/ (- (* s2x
                      (- (point-y (2d-vector-start v1))
                         (point-y (2d-vector-start v2))))
                   (* s2y
                      (- (point-x (2d-vector-start v1))
                         (point-x (2d-vector-start v2)))))
                (+ (* (- s2x) s1y)
                   (* s1x s2y)))))
    (when (and (<= 0 s 1) (<= 0 tt 1))
      (make-point :x (+ (point-x (2d-vector-start v1))
                        (* tt s1x))
                  :y (+ (point-y (2d-vector-start v1))
                        (* tt s1y))
                  :z nil))))

;;; task 4.b
(defun approximate (fun min max epsilon)
  (let ((half (* 0.5 (- max min))))
    (if (< (- max min) epsilon)
        half
        (let ((fmin (funcall fun min))
              (fmax (funcall fun max))
              (fhalf (funcall fun half)))
          (cond ((< 0 (* fmin fmax))
                 (format nil "wrong range (min max): min = ~A, max = ~A" min max))
                ((zerop fhalf)
                 half)
                ((< 0 (* fmin fhalf))
                 (approximate fun half max eps))
                ((< 0 (* fmax fhalf))
                 (approximate fun min half epsilon))
                (t nil))))))


;;; tsak 5
(defun horner (x &rest args)
  (cond ((null args)
         args)
        ((null (cdr args))
         (car args))
        (t (horner-core x (cdr args) (car args)))))

(defun horner-core (x args acc)
  (if (null args)
      (float acc)
      (horner-core x (cdr args) (+ (* acc x) (car args)))))

;;; task 6
(log (1+ most-positive-fixnum) 2)

;;; task 7
;; sbcl
;; most-negative-double-float
;; most-positive-double-float
;; most-negative-long-float
;; most-positive-long-float
;; most-negative-short-float
;; most-positive-short-float
;; most-negative-single-float
;; most-positive-single-float
