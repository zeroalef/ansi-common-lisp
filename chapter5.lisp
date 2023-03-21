(defconstant +month+
  #(0 31 59 90 120 151 181 212 243 273 304 334 365))

(defconstant +yzero+ 2000)

(defun leap? (y)
  (and (zerop y)
       (or (zerop (mod y 400))
           (not (zerop (mod y 100))))))

(defun date->num (d m y)
  (+ (1- d)
     (month-num m y)
     (year-num y)))

(defun month-num (m y)
  (+ (svref +month+ (1- m))
     (if (and (> m 2) (leap? y)) 1 0)))

(defun year-num (y)
  (let ((d 0))
    (if (>= y +yzero+)
        (dotimes (i (- y +yzero+) d)
          (incf d (year-days (+ +yzero+ i))))
        (dotimes (i (- +yzero+ y) (- d))
          (incf d (year-days (+ y i)))))))

(defun year-days (y)
  (if (leap? y) 366 365))


(defun num->date (n)
  (multiple-value-bind (y left)
      (num-year n)
    (multiple-value-bind (m d)
        (num-month left y)
      (values d m y))))

(defun num-year (n)
  (if (minusp n)
      (do* ((y (- +yzero+ 1) (1- y))
            (d (- (year-days y)) (- d (year-days y))))
           ((<= d n) (values y (- n d))))
      (do* ((y +yzero+ (1+ y))
            (prev 0 d)
            (d (year-days y) (+ d (year-days y))))
           ((> d n) (values y (- n prev))))))

(defun num-month (n y)
  (if (leap? y)
      (cond ((= n 59) (values 2 29))
            ((> n 59) (nmon (1- n)))
            (t (nmon n)))
      (nmon n)))

(defun nmon (n)
  (let ((m (position n +month+ :test #'<)))
    (values m (+ 1 (- n (svref +month+ (1- m)))))))

(defun date+ (d m y n)
  (num->date (+ (date->num d m y) n)))



;;; Task 1 a
(defun foo (x)
  ((lambda (y) (cons y y))
   (car x)))
;; CL-USER> (foo '(1 2 3))
;; (1 . 1)
;; CL-USER>

;;; Task 1 b
(defun bar (x z)
  ((lambda (w)
   ((lambda (y)
      (cons w y))
    (+ w z)))
   (car x)))

;; CL-USER> (bar '(1 2 3) 5)
;; (1 . 6)
;; CL-USER>


;;; task 2
;;; return position of `x` on `y` list
(defun mistery (x y)
  (cond ((null y) y)
        ((eql (car y) x) 0)
        (t (let ((z (mistery x (cdr y))))
             (and z (+ z 1))))))


;;; task 3
(defun stranger-square (x)
  (when (and (integerp x) (< 0 x 6))
    (* x x)))

;;; task 4
(defun new-month-num (m y)
  (+ (case m
       (1 0)
       (2 31)
       (3 59)
       (4 90)
       (5 120)
       (6 151)
       (7 181)
       (8 212)
       (9 243)
       (10 273)
       (11 304)
       (12 334)
       (13 365))
     (if (and (> m 2) (leap? y)) 1 0)))


;;; task 5
(defun precedes-recursive (item vec)
  (let ((len (length vec)))
    (when (> len 1)
      (labels
          ((fun (index acc)
             (cond ((>= index len) acc)
                   ((eql item (aref vec index))
                    (fun (1+ index)
                         (adjoin (aref vec (1- index))
                               acc)))
                   (t (fun (1+ index) acc)))))
        (fun 1 nil)))))


(defun precedes-iterative (item vec)
  (let ((len (length vec)))
    (when (> len 1)
      (do ((index 1 (1+ index))
           (acc nil (if (eql (aref vec index) item)
                        (adjoin (aref vec (1- index)) acc)
                        acc)))
          ((>= index len) acc)))))


;;; task 6
(defun intersprese-recursive (item lst)
  (destructuring-bind (head . tail) lst
    (if (null tail)
        lst
        (cons head (cons item (intersprese item tail))))))

(defun intersprese-iterative (item lst)
  (let (acc)
    (dolist (i (reverse lst) (cdr acc))
      (progn
        (push i acc)
        (push item acc)))))

;;; task 7
(defun seq+1-recursive (lst)
  (when (and lst (listp lst) (cdr lst))
    (labels ((fun (ls acc)
               (destructuring-bind (head . tail) ls
                 (if (null tail)
                     acc
                     (fun tail (and acc (= 1 (- (car tail) head))))))))
      (fun lst t))))

(defun seq+1-do (lst)
  (when (and lst (listp lst) (cdr lst))
    (do ((left lst (cdr left))
         (right (cdr lst) (cdr right)))
        ((not right) t)
      (when (/= 1 (- (car right) (car left)))
        (return nil)))))

(defun seq+1-mapc (lst)
  (when (and lst (listp lst) (cdr lst))
    (block nil
      (let ((tmp (car lst)))
        (mapc #'(lambda (item)
                  (if (/= 1 (- item tmp))
                      (return nil)
                      (setf tmp item)))
              (cdr lst))
        t))))

;;; task 8
(defun min-max (vec &key
                      (index 0)
                      (min (aref vec 0))
                      (max (aref vec 0))
                      (len (length vec)))
  (if (= index len)
      (values min max)
      (let ((tmp (aref vec index)))
        (min-max vec
                 :index (1+ index)
                 :min (if (< tmp min) tmp min)
                 :max (if (> tmp max) tmp max)
                 :len len))))


;;; use catch and throw
(defun shortest-path (start end net)
  (if (eql start end)
      (list start)
    (catch 'found
      (bfs end (list (list start)) net))))

(defun bfs (end queue net)
  (if (null queue)
      nil
    (let* ((path (car queue)) (node (car path)))
      (bfs end
           (append (cdr queue)
                   (new-paths path node net end))
           net))))

(defun new-paths (path node net end)
  (mapcar #'(lambda (n)
              (let ((path1 (cons n path)))
                (if (eql n end)
                    (throw 'found (reverse path1))
                  path1)))
          (cdr (assoc node net))))
