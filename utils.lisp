(defun add1 (x) (+ x 1))
(defun sub1 (x) (- x 1))
(defun square (x) (* x x))


(defun range (end &optional
                    (optional-end 0 optend-supplied-p)
                    (increase 1))
  "Frontend function to generate a range list"
  (declare (type integer end))
  (declare (type integer optional-end))
  (declare (type integer increase))
  (if optend-supplied-p
      (cond ((> end optional-end)
             (if (or (plusp increase)
                     (zerop increase))
                 (format nil "bad value increase: ~a" increase)
                 (range-core end optional-end increase #'<= nil)))
            ((< end optional-end)
             (if (or (zerop increase)
                     (minusp increase))
                 (format nil "bad value increase: ~a" increase)
                 (range-core end optional-end increase #'>= nil)))
            (t nil))
      (range-core 0 end increase #'>= nil)))


(defun range-core (start end increase predicate acc)
  (if (funcall predicate start end)
      (reverse acc)
      (range-core (+ start increase) end increase predicate (cons start acc))))


(defun single? (lst)
  (and (consp lst)
       (null (cdr lst))))

(defun append-item (lst item)
  (append lst `(,item)))

(defun map-int (fn n)
  (let ((acc nil))
    (dotimes (i n)
      (push (funcall fn i) acc))
    (nreverse acc)))

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (item lst)
      (when (funcall fn item) (push item acc)))
    (nreverse acc)))

(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall fn wins)))
        (dolist (item (cdr lst))
          (let ((score (funcall fn item)))
            (if (> score max)
                (setf wins item
                      max score))))
        (values wins max))))


(defun compose (&rest funs)
  (destructuring-bind (fn . rest) (reverse funs)
    #'(lambda (&rest args)
        (reduce #'(lambda (v f) (funcall f v))
                rest
                :initial-value (apply fn args)))))


(defun disjoin (fn &rest funs)
  (if (null funs)
      fn
      (let ((disj (apply #'disjoin funs)))
        #'(lambda (&rest args)
            (or (apply fn args)
                (apply disj args))))))

(defun conjoin (fn &rest funs)
  (if (null funs)
      fn
      (let ((conj (apply #'conjoin funs)))
        #'(lambda (&rest args)
            (or (apply fn args)
                (apply conj args))))))


(defun curry (fn &rest args)
  #'(lambda (&rest args2)
      (apply fn (append args args2))))

(defun rcurry (fn &rest args)
  #'(lambda (&rest args2)
      (apply fn (append args2 args))))

