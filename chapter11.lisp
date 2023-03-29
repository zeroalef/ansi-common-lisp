(defclass rectangle ()
  ((height
    :accessor rectangle-height
    :initarg :height
    :initform 0
    :type 'real
    :documentation "describe field string")
   (width
    :accessor rectangle-width
    :initarg :width
    :initform 0)))

(defclass circle ()
  ((radius
    :accessor circle-radius
    :initarg :radius
    :initform 0)
   (center
    :accessor cirsle-center
    :initarg :center
    :initform (cons 0 0))))

(defmethod area ((x rectangle))
  (* (rectangle-height x)
     (rectangle-width x)))

(defmethod area ((x circle))
  (* pi (expt (circle-radius x) 2)))

(defclass graphic ()
  ((color
    :accessor graphic-color
    :initarg :color)
   (visible
    :accessor graphic-visible
    :initarg :visible)))

(defclass screen-circle (circle graphic)
  ((color
    :initform 'purple)))


(defmethod combine (x y)
  (list x y))

(defclass stuff ()
  ((name
    :accessor name
    :initarg :name)))

(defclass ice-cream (stuff)
  ())

(defclass topping (stuff)
  ())



(defmethod combine ((ic ice-cream) (top topping))
  (format nil "~A ice-cream with ~A topping."
          (name ic)
          (name top)))

(defmethod combine ((ic ice-cream) x)
  (format nil "~A ice-cream with ~A topping."
          (name ic)
          x))



(defclass speaker ()
  ())

(defclass intellectual (speaker)
  ())

(defclass courtier (speaker)
  ())

(defmethod speak ((s speaker) string)
  (format t "~A" string))

(defmethod speak :before ((s speaker) string)
  (princ "I think"))

(defmethod speak :before ((i intellectual) string)
  (princ "Perhaps "))

(defmethod speak :after ((i intellectual) string)
  (princ " in some sence"))

;; CL-USER> (speak (make-instance 'intellectual) "I'm hungry")
;; Perhaps I thinkI'm hungry in some sence



(defmethod speak :around ((c courtier) string)
  (format t "Does the king believe that ~A? " string)
  (if (eql (read) 'yes)
      (if (next-method-p) (call-next-method))
      (format t "Indeed, it is a preposterous idea. ~%"))
  'bow)

;; CL-USER> (speak (make-instance 'courtier) "kings will last")
;; Does the king believe that kings will last? yes
;; I thinkkings will last
;; BOW
;; CL-USER> (speak (make-instance 'courtier) "the world is round")
;; Does the king believe that the world is round? no
;; Indeed, it is a preposterous idea.
;; BOW
;; CL-USER>

(defgeneric price (x)
  ;; all variants fo metho-combination
  ;; + and append list max min nconc or progn
  (:method-combination +))

(defclass jacket ()
  ())
(defclass trousers ()
  ())
(defclass suit (jacket trousers)
  ())

(defmethod price + ((jk jacket)) 350)
(defmethod price + ((tr trousers)) 200)


;; (defpackage "CTR"
;;   (:use "COMMON-LISP")
;;   (:export "COUNTER" "INCREMENT" "CLEAR"))

;; (in-package ctr)

;; (defclass counter ()
;;   ((state :initform 0)))

;; (defmethod increment ((c counter))
;;   (incf (slot-value c ’state)))

;; (defmethod clear ((c counter))
;;   (setf (slot-value c ’state) 0))

;;; task 1 at the file begin
;;; task 2
(defclass point ()
  ((x
    :accessor pt-x
    :initarg :x
    :initform 0
    :type 'number)
   (y
    :accessor pt-y
    :initarg :y
    :initform 0
    :type 'number)
   (z
    :accessor pt-z
    :initarg :z
    :initform 0
    :type 'number)))

(defclass surface ()
  ((color
    :accessor surpface-color
    :initarg :color)))

(defclass sphere (surface)
   ((radius
     :accessor  sphere-radius
     :initarg :radius
     :initform 0)
    (center
     :accessor sphere-center
     :initarg :center
     :initform (make-instance 'point :x 0 :y 0 :z 0))))



(defparameter *world* nil)

(defun defsphere (x y z r c)
  (let ((s (make-instance
            'sphere
            :radius r
            :center (make-instance 'point :x x :y y :z z)
            :color c)))
    (push s *world*)
    s))

(defmethod square ((x number))
  (* x x))
;; for a compiler only
(defmethod unit-vector ((x number) (y number) (z number))
  0)
;; for a compiler only
(defmethod minroot ((a number) (b number) (c number))
  0)

(defmethod intersect ((s sphere) (pt point) xr yr zr)
  (let* ((c (sphere-center s))
         (n (minroot (+ (square xr)
                        (square yr)
                        (square zr))
                     (* 2 (+ (* (- (pt-x pt) (pt-x c)) xr)
                             (* (- (pt-y pt) (pt-y c)) yr)
                             (* (- (pt-z pt) (pt-z c)) zr)))
                     (+ (square (- (pt-x pt) (pt-x c)))
                        (square (- (pt-y pt) (pt-y c)))
                        (square (- (pt-z pt) (pt-z c)))
                        (- (square (sphere-radius s)))))))
    (if n
        (make-instance 'point
                       :x  (+ (pt-x pt) (* n xr))
                       :y  (+ (pt-y pt) (* n yr))
                       :z  (+ (pt-z pt) (* n zr))))))


(defmethod normal ((s sphere) (pt point))
  (let ((c (sphere-center s)))
    (unit-vector (- (pt-x c) (pt-x pt))
                 (- (pt-y c) (pt-y pt))
                 (- (pt-z c) (pt-z pt)))))

;;; task 3
;; a.
;; order of specification (descending): a, c, d, e, f, g, h
;;     (h)
;;     / \
;;(e) (f)(g)
;;   \ | /
;;(c) (d)
;;  \ /
;;  (a)
;; b.
;; order of specification (descending): b, d, e, f, g, h, c
;;      ( h )
;;      /  \
;;(e) (f) (g)
;;  \  |  /
;;    (d)   (c)
;;      \  /
;;      (a )


;;; task 4
(defun precedence (n))
(defun methods (gfun))
(defun specializations (meth))

(defun most-spec-app-meth (gfun av)
  (let ((classlist (mapcar #'precedence av)))
    (dolist (meth (methods gfun))
      (if (do ((i 0 (1+ i)) (spec (specializations meth) (cdr spec)))
              ((not spec) t)
            (or (member (car spec) (nth i classlist) :test #'equal)
                (return)))
          (return-from most-spec-app-meth meth)))))


;;; task 5
(defparameter *global-counter* 0)

(defvar *area-counter* 0)
(defmethod area :before (obj)
  (declare (ignore obj))
  (incf *area-counter*))
